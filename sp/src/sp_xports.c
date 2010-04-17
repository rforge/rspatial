#define USING_R 1

#include "sp.h"
/* remember to touch local_stubs.c */

SEXP SP_PREFIX(Polygon_c)(SEXP coords, SEXP n, SEXP ihole) {

    SEXP SPans, labpt, Area, ringDir, hole;
    double area, xc, yc;
    double *x, *y;
    int pc=0, rev=FALSE;
    int i, ii, nn=INTEGER_POINTER(n)[0];
    SEXP valid;

    SP_PREFIX(spRFindCG_c)(n, coords, &xc, &yc, &area);
    if (abs(area) < DOUBLE_EPS) {
        if (!R_FINITE(xc) || !R_FINITE(xc)) {
            if (nn == 1) {
                xc = NUMERIC_POINTER(coords)[0];
                yc = NUMERIC_POINTER(coords)[1];
            } else if (nn == 2) {
              xc = (NUMERIC_POINTER(coords)[0]+NUMERIC_POINTER(coords)[1])/2.0;
              yc = (NUMERIC_POINTER(coords)[2]+NUMERIC_POINTER(coords)[3])/2.0;
            } else if (nn > 2) {
              xc = (NUMERIC_POINTER(coords)[0] +
                NUMERIC_POINTER(coords)[(nn-1)])/2.0;
              yc = (NUMERIC_POINTER(coords)[nn] +
                NUMERIC_POINTER(coords)[nn+(nn-1)])/2.0;
            }
        }
    }

    PROTECT(SPans = NEW_OBJECT(MAKE_CLASS("Polygon"))); pc++;

    PROTECT(ringDir = NEW_INTEGER(1)); pc++;
    INTEGER_POINTER(ringDir)[0] = (area > 0.0) ? -1 : 1;

/* RSB 100126 fixing hole assumption
 thanks to Javier Munoz for report */

    if (INTEGER_POINTER(ihole)[0] == NA_INTEGER) { // trust ring direction
        if (INTEGER_POINTER(ringDir)[0] == 1) {
            INTEGER_POINTER(ihole)[0] = 0;
        } else if (INTEGER_POINTER(ringDir)[0] == -1) {
            INTEGER_POINTER(ihole)[0] = 1;
        }
    } else { // trust hole
        if (INTEGER_POINTER(ihole)[0] == 1 && 
            INTEGER_POINTER(ringDir)[0] == 1) {
            rev = TRUE;
            INTEGER_POINTER(ringDir)[0] = -1;
        }
        if (INTEGER_POINTER(ihole)[0] == 0 && 
            INTEGER_POINTER(ringDir)[0] == -1) {
            rev = TRUE;
            INTEGER_POINTER(ringDir)[0] = 1;
        }
    }
    PROTECT(hole = NEW_LOGICAL(1)); pc++;
    if (INTEGER_POINTER(ihole)[0] == 1) LOGICAL_POINTER(hole)[0] = TRUE;
    else LOGICAL_POINTER(hole)[0] = FALSE;

    if (rev) {
        x = (double *) R_alloc((size_t) nn, sizeof(double));
        y = (double *) R_alloc((size_t) nn, sizeof(double));
       for (i=0; i<nn; i++) {
           x[i] = NUMERIC_POINTER(coords)[i];
           y[i] = NUMERIC_POINTER(coords)[i+nn];
       }
       for (i=0; i<nn; i++) {
           ii = (nn-1)-i;
           NUMERIC_POINTER(coords)[ii] = x[i];
           NUMERIC_POINTER(coords)[ii+nn] = y[i];
       }
    }
    
    SET_SLOT(SPans, install("coords"), coords);

    PROTECT(labpt = NEW_NUMERIC(2)); pc++;
    NUMERIC_POINTER(labpt)[0] = xc;
    NUMERIC_POINTER(labpt)[1] = yc;
    SET_SLOT(SPans, install("labpt"), labpt);

    PROTECT(Area = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(Area)[0] = fabs(area);
    SET_SLOT(SPans, install("area"), Area);

    SET_SLOT(SPans, install("hole"), hole);
    SET_SLOT(SPans, install("ringDir"), ringDir);

    PROTECT(valid = SP_PREFIX(Polygon_validate_c)(SPans)); pc++;
    if (!isLogical(valid)) {
        UNPROTECT(pc);
        if (isString(valid)) error(CHAR(STRING_ELT(valid, 0)));
        else error("invalid Polygon object");
    }

    UNPROTECT(pc);
    return(SPans);

}

SEXP SP_PREFIX(Polygon_validate_c)(SEXP obj) {

    int pc=0;
    int n;
    SEXP coords, labpt, ans;

    coords = GET_SLOT(obj, install("coords"));
    n = INTEGER_POINTER(getAttrib(coords, R_DimSymbol))[0];
    if (NUMERIC_POINTER(coords)[0] != NUMERIC_POINTER(coords)[n-1]
        || NUMERIC_POINTER(coords)[n] != NUMERIC_POINTER(coords)[(2*n)-1]) {
       PROTECT(ans = NEW_CHARACTER(1)); pc++;
       SET_STRING_ELT(ans, 0,
           COPY_TO_USER_STRING("ring not closed"));
       UNPROTECT(pc);
       return(ans);
    }
    labpt = GET_SLOT(obj, install("labpt"));
    if (!R_FINITE(NUMERIC_POINTER(labpt)[0]) ||
        !R_FINITE(NUMERIC_POINTER(labpt)[1])) {
        PROTECT(ans = NEW_CHARACTER(1)); pc++;
        SET_STRING_ELT(ans, 0,
           COPY_TO_USER_STRING("infinite label point"));
       UNPROTECT(pc);
       return(ans);
    }
    PROTECT(ans = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(ans)[0] = TRUE;
    UNPROTECT(pc);
    return(ans);

}

SEXP SP_PREFIX(Polygons_c)(SEXP pls, SEXP ID) {

    SEXP ans, labpt, Area, plotOrder, crds, pl, n, hole;
    int nps, i, pc=0, sumholes;
    double *areas, *areaseps, fuzz;
    int *po, *holes;
    SEXP valid;

    nps = length(pls);
    fuzz = R_pow(DOUBLE_EPS, (2.0/3.0));
    areas = (double *) R_alloc((size_t) nps, sizeof(double));
    areaseps = (double *) R_alloc((size_t) nps, sizeof(double));
    holes = (int *) R_alloc((size_t) nps, sizeof(int));

    for (i=0, sumholes=0; i<nps; i++) {
        areas[i] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls, i),
            install("area")))[0]; 
        holes[i] = LOGICAL_POINTER(GET_SLOT(VECTOR_ELT(pls, i),
            install("hole")))[0];
         areaseps[i] = holes[i] ? areas[i] + fuzz : areas[i];
         sumholes += holes[i];
    }
    po = (int *) R_alloc((size_t) nps, sizeof(int));
    if (nps > 1) {
        for (i=0; i<nps; i++) po[i] = i + R_OFFSET;
        revsort(areaseps, po, nps);
    } else {
        po[0] = 1;
    }

    if (sumholes == nps) {
        crds = GET_SLOT(VECTOR_ELT(pls, (po[0] - R_OFFSET)), install("coords"));
        PROTECT(n = NEW_INTEGER(1)); pc++;
        INTEGER_POINTER(n)[0] = INTEGER_POINTER(getAttrib(crds,
            R_DimSymbol))[0];
        PROTECT(hole = NEW_LOGICAL(1)); pc++;
        LOGICAL_POINTER(hole)[0] = FALSE;
        pl = SP_PREFIX(Polygon_c)(crds, n, hole);
/* bug 100417 Patrick Giraudoux */
        holes[po[0] - R_OFFSET] = LOGICAL_POINTER(hole)[0];
        SET_VECTOR_ELT(pls, (po[0] - R_OFFSET), pl);
    }

    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Polygons"))); pc++;
    SET_SLOT(ans, install("Polygons"), pls);
    SET_SLOT(ans, install("ID"), ID);

    PROTECT(Area = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(Area)[0] = 0.0;
    for (i=0; i<nps; i++) {
        NUMERIC_POINTER(Area)[0] += holes[i] ? 0.0 : fabs(areas[i]);
    }
    SET_SLOT(ans, install("area"), Area);

    PROTECT(plotOrder = NEW_INTEGER(nps)); pc++;
    for (i=0; i<nps; i++) INTEGER_POINTER(plotOrder)[i] = po[i];
    SET_SLOT(ans, install("plotOrder"), plotOrder);

    PROTECT(labpt = NEW_NUMERIC(2)); pc++;
    NUMERIC_POINTER(labpt)[0] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls,
        (po[0]-1)), install("labpt")))[0];
    NUMERIC_POINTER(labpt)[1] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls,
        (po[0]-1)), install("labpt")))[1];
    SET_SLOT(ans, install("labpt"), labpt);

    PROTECT(valid = SP_PREFIX(Polygons_validate_c)(ans)); pc++;
    if (!isLogical(valid)) {
        UNPROTECT(pc);
        if (isString(valid)) error(CHAR(STRING_ELT(valid, 0)));
        else error("invalid Polygons object");
    }

    UNPROTECT(pc);
    return(ans);
}

SEXP SP_PREFIX(Polygons_validate_c)(SEXP obj) {

    int pc=0;
    int i, n;
    SEXP Pls, labpt, ans;
    char *cls="Polygon";

    PROTECT(Pls = GET_SLOT(obj, install("Polygons"))); pc++;
    n = length(Pls);
    for (i=0; i<n; i++) {
        if (strcmp(CHAR(STRING_ELT(getAttrib(VECTOR_ELT(Pls, i),
           R_ClassSymbol), 0)), cls) != 0) {
              PROTECT(ans = NEW_CHARACTER(1)); pc++;
              SET_STRING_ELT(ans, 0,
              COPY_TO_USER_STRING("Polygons slot contains non-Polygon object"));
              UNPROTECT(pc);
              return(ans);
        }
    }

    if (n != length(GET_SLOT(obj, install("plotOrder")))) {
        PROTECT(ans = NEW_CHARACTER(1)); pc++;
        SET_STRING_ELT(ans, 0,
           COPY_TO_USER_STRING("plotOrder and Polygons differ in length"));
        UNPROTECT(pc);
        return(ans);
    }

    labpt = GET_SLOT(obj, install("labpt"));
    if (!R_FINITE(NUMERIC_POINTER(labpt)[0]) ||
      !R_FINITE(NUMERIC_POINTER(labpt)[1])) {
      PROTECT(ans = NEW_CHARACTER(1)); pc++;
      SET_STRING_ELT(ans, 0,
         COPY_TO_USER_STRING("infinite label point"));
      UNPROTECT(pc);
      return(ans);
    }
    PROTECT(ans = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(ans)[0] = TRUE;
    UNPROTECT(pc);
    return(ans);

}

SEXP SP_PREFIX(SpatialPolygons_c)(SEXP pls, SEXP pO, SEXP p4s) {

    SEXP ans, bbox;
    int pc=0;

    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("SpatialPolygons"))); pc++;
    SET_SLOT(ans, install("polygons"), pls);
    SET_SLOT(ans, install("proj4string"), p4s);

    if (pO == R_NilValue) {
        SET_SLOT(ans, install("plotOrder"), SP_PREFIX(SpatialPolygons_plotOrder_c)(pls));
    } else {
        SET_SLOT(ans, install("plotOrder"), pO);
    }

    PROTECT(bbox = SP_PREFIX(bboxCalcR_c(pls))); pc++;
    SET_SLOT(ans, install("bbox"), bbox);

    UNPROTECT(pc);
    return(ans);

}

SEXP SP_PREFIX(SpatialPolygons_plotOrder_c)(SEXP pls) {

    SEXP plotOrder;
    int pc=0, ng, i;
    int *po;
    double *areas;

    ng = length(pls);
    areas = (double *) R_alloc((size_t) ng, sizeof(double));
    po = (int *) R_alloc((size_t) ng, sizeof(int));
    for (i=0; i<ng; i++) {
        areas[i] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls, i),
            install("area")))[0]; 
        po[i] = i + R_OFFSET;
    }
    revsort(areas, po, ng);
    PROTECT(plotOrder = NEW_INTEGER(ng)); pc++;
    for (i=0; i<ng; i++) INTEGER_POINTER(plotOrder)[i] = po[i];

    UNPROTECT(pc);
    return(plotOrder);

}

SEXP SP_PREFIX(SpatialPolygons_validate_c)(SEXP obj) {

    int pc=0;
    int i, n;
    SEXP pls, ans;
    char *cls="Polygons";

    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    n = length(pls);
    for (i=0; i<n; i++) {
        if (strcmp(CHAR(STRING_ELT(getAttrib(VECTOR_ELT(pls, i),
           R_ClassSymbol), 0)), cls) != 0) {
             PROTECT(ans = NEW_CHARACTER(1)); pc++;
             SET_STRING_ELT(ans, 0,
             COPY_TO_USER_STRING("polygons slot contains non-Polygons object"));
             UNPROTECT(pc);
             return(ans);
        }
    }

    if (n != length(GET_SLOT(obj, install("plotOrder")))) {
        PROTECT(ans = NEW_CHARACTER(1)); pc++;
        SET_STRING_ELT(ans, 0,
           COPY_TO_USER_STRING("plotOrder and polygons differ in length"));
        UNPROTECT(pc);
        return(ans);
    }

    PROTECT(ans = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(ans)[0] = TRUE;
    UNPROTECT(pc);
    return(ans);

}

SEXP SP_PREFIX(SpatialPolygons_getIDs_c)(SEXP obj) {

    int pc=0;
    int i, n;
    SEXP pls, IDs;

    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    n = length(pls);
    PROTECT(IDs = NEW_CHARACTER(n)); pc++;
    for (i=0; i<n; i++)
        SET_STRING_ELT(IDs, i, STRING_ELT(GET_SLOT(VECTOR_ELT(pls, i),
            install("ID")), 0));
      
    UNPROTECT(pc);
    return(IDs);

}

SEXP SP_PREFIX(bboxCalcR_c)(SEXP pls) {

    SEXP ans, dim, dimnames, Pl, crds;
    double UX=-DBL_MAX, LX=DBL_MAX, UY=-DBL_MAX, LY=DBL_MAX;
    int i, j, k, n, npls, npl, pc=0;
    double x, y;

    npls = length(pls);
    for (i=0; i<npls; i++) {
        Pl = GET_SLOT(VECTOR_ELT(pls, i), install("Polygons"));
        npl = length(Pl);
        for (j=0; j<npl; j++) {
            crds = GET_SLOT(VECTOR_ELT(Pl, j), install("coords"));
            n = INTEGER_POINTER(getAttrib(crds, R_DimSymbol))[0];
            for (k=0; k<n; k++) {
               x = NUMERIC_POINTER(crds)[k];
               y = NUMERIC_POINTER(crds)[k+n];
               if (x > UX) UX = x;
               if (y > UY) UY = y;
               if (x < LX) LX = x;
               if (y < LY) LY = y;
            }
        }
    }

    PROTECT(ans = NEW_NUMERIC(4)); pc++;
    NUMERIC_POINTER(ans)[0] = LX;
    NUMERIC_POINTER(ans)[1] = LY;
    NUMERIC_POINTER(ans)[2] = UX;
    NUMERIC_POINTER(ans)[3] = UY;
    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = 2;
    INTEGER_POINTER(dim)[1] = 2;
    setAttrib(ans, R_DimSymbol, dim);
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 0, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 1, COPY_TO_USER_STRING("y"));
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("min"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("max"));
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(pc);
    return(ans);

}

void SP_PREFIX(spRFindCG_c)( SEXP n, SEXP coords, double *xc, double *yc, 
		double *area ) {

	int i, nn;
	tPointd *P;
	tPointd CG;
	double Areasum2;
	nn = INTEGER_POINTER(n)[0];
	P = (tPointd *) R_alloc(nn, sizeof(tPointd));
	for (i=0; i<nn; i++) {
		P[i][0] = NUMERIC_POINTER(coords)[i];
		P[i][1] = NUMERIC_POINTER(coords)[i+nn];
	}
	SP_PREFIX(FindCG)(nn, P, CG, &Areasum2);
	xc[0] = CG[0];
	yc[0] = CG[1];
	area[0] = Areasum2/2;
	return;
}

void     SP_PREFIX(FindCG)( int n, tPointd *P, tPointd CG, double *Areasum2)
{
        int     i;
        double  A2;        /* Partial area sum */    
	tPointd Cent3;

	CG[0] = 0;
	CG[1] = 0;
        Areasum2[0] = 0;
	for (i = 1; i < n-1; i++) {
	        SP_PREFIX(Centroid3)( P[0], P[i], P[i+1], Cent3 );
	        A2 =  SP_PREFIX(Area2)( P[0], P[i], P[i+1]);
		CG[0] += A2 * Cent3[0];
		CG[1] += A2 * Cent3[1];
		Areasum2[0] += A2;
	      }
        CG[0] /= 3 * Areasum2[0];
        CG[1] /= 3 * Areasum2[0];
	return;
}
/*
	Returns three times the centroid.  The factor of 3 is
	left in to permit division to be avoided until later.
*/
void    SP_PREFIX(Centroid3)( tPointd p1, tPointd p2, tPointd p3, tPointd c )
{
        c[0] = p1[0] + p2[0] + p3[0];
        c[1] = p1[1] + p2[1] + p3[1];
	return;
}
/* 
        Returns twice the signed area of the triangle determined by a,b,c,
        positive if a,b,c are oriented ccw, and negative if cw.
*/
double     SP_PREFIX(Area2)( tPointd a, tPointd b, tPointd c )
{
	double area;
	area = (b[0] - a[0]) * (c[1] - a[1]) - (c[0] - a[0]) * (b[1] - a[1]);
	return(area);
}


SEXP SP_PREFIX(comment2comm)(SEXP obj) {
    SEXP ans, comment;
    int pc=0, ns, i, j, jj, k;
    char buf[BUFSIZE], s[15];
    int *c, *nss, *co, *coo;

    PROTECT(comment = getAttrib(obj, install("comment"))); pc++;
    if (comment == R_NilValue) {
        UNPROTECT(pc);
        return(R_NilValue);
    }
    strcpy(buf, CHAR(STRING_ELT(comment, 0)));

    i = 0;
    ns = 0;
    while (buf[i] != '\0') {
        if (buf[i] == ' ') ns++;
        i++;
    }
    k = (int) strlen(buf);

   
    nss = (int *) R_alloc((size_t) (ns+1), sizeof(int));
    c = (int *) R_alloc((size_t) (ns+1), sizeof(int));
    i = 0;
    j = 0;
    while (buf[i] != '\0') {
        if (buf[i] == ' ') {
            nss[j] = i; j++;
        }
        i++;
    }
    nss[(ns)] = k;

    s[0] = '\0';
    if (nss[0] > 15) error("comment2comm: buffer overflow");
    strncpy(s, &buf[0], (size_t) nss[0]);
    s[nss[0]] = '\0';

    c[0] = atoi(s);
    for (i=0; i<ns; i++) {
        k = nss[(i+1)]-(nss[i]+1);
        if (k > 15) error("comment2comm: buffer overflow");
        strncpy(s, &buf[(nss[i]+1)], (size_t) k);
        s[k] = '\0';
        c[i+1] = atoi(s);
    }

    for (i=0, k=0; i<(ns+1); i++) if (c[i] == 0) k++;
    
    PROTECT(ans = NEW_LIST((k))); pc++;
    co = (int *) R_alloc((size_t) k, sizeof(int));
    coo = (int *) R_alloc((size_t) k, sizeof(int));
    for (i=0; i<k; i++) co[i] = 1;

    for (i=0, j=0; i<(ns+1); i++)
        if (c[i] == 0) coo[j++] = i + R_OFFSET;

    for (i=0; i<k; i++)
        for (j=0; j<(ns+1); j++)
            if ((c[j]) == coo[i]) co[i]++;

    for (i=0; i<k; i++) SET_VECTOR_ELT(ans, i, NEW_INTEGER(co[i]));

    for (i=0; i<k; i++) {
        jj = 0;
        INTEGER_POINTER(VECTOR_ELT(ans, i))[jj++] = coo[i];
        if (co[i] > 1) {
            for (j=0; j<(ns+1); j++)
                if (c[j] == coo[i])
                    INTEGER_POINTER(VECTOR_ELT(ans, i))[jj++] = j + R_OFFSET;
        }
    }

    UNPROTECT(pc);
    return(ans);
}

void SP_PREFIX(comm2comment)(char *buf, int *comm, int nps) {
    char cbuf[15];
    int i;

    sprintf(buf, "%d", comm[0]);
    for (i=1; i<nps; i++) {
        sprintf(cbuf, " %d", comm[i]);
        strcat(buf, cbuf);
    }
    return;
}

/* remember to touch local_stubs.c */
