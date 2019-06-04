#include <R.h>
#include <stddef.h>
#include "exactsum.h"

/* Written by Mikko Korpela. */

/* Common building block for functions below */
#define GROWEXP_Body							\
    /* Grow-Expansion(expansion, a) */					\
    readptr = expansion;						\
    writeptr = expansion;						\
    while(readptr != NULL && readptr->valid == TRUE) {			\
	/* Updating readptr is easy: just do it once in the loop */	\
	/* and stay ahead of writeptr */				\
	b = readptr->data;						\
	readptr = readptr->next;					\
	/* Two-Sum(a,b): x + y == a + b */				\
	x = a + b;							\
	b_virtual = x - a;						\
	a_virtual = x - b_virtual;					\
	b_roundoff = b - b_virtual;					\
	a_roundoff = a - a_virtual;					\
	y = a_roundoff + b_roundoff;					\
	if(y != 0){							\
	    writeptr->data = y;						\
	    /* Loosely specified invariant: always have writeptr */	\
	    /* point to a writable location */				\
	    if(writeptr->next != NULL){					\
		writeptr = writeptr->next;				\
	    } else{							\
	        writeptr->next =					\
		    (listnode *) R_alloc(1, sizeof(listnode));		\
		writeptr = writeptr->next;				\
		writeptr->next = NULL;					\
	    }								\
	}								\
	a = x;								\
    }									\
    writeptr->data = a;							\
    writeptr->valid = TRUE;						\
    									\
    /* The possible tail of the list is effectively cut (number of */	\
    /* non-zero elements may decrease), but any allocated space */	\
    /* remains there */							\
    if(writeptr->next != NULL)						\
	writeptr->next->valid = FALSE;

dplr_double msum(double *array, size_t n, listnode *expansion){
    size_t k;
    dplr_double a,b,a_virtual,b_virtual,a_roundoff,b_roundoff,x,y,total;
    listnode *readptr, *writeptr;

    /* Old data are not valid anymore */
    expansion->valid = FALSE;

    /* Loop through array */
    for(k=0; k<n; k++) {
	a = array[k];
	GROWEXP_Body
    }

    /* Add together the elements of the expansion */
    total = 0;
    while(expansion != NULL && expansion->valid == TRUE){
	total += expansion->data;
	expansion = expansion->next;
    }
    return(total);
}

/* Cumulative sum, overwrites array */
dplr_double cumsum(double *array, size_t n, listnode *expansion){
    size_t k;
    dplr_double a,b,a_virtual,b_virtual,a_roundoff,b_roundoff,x,y,total;
    listnode *readptr, *writeptr, *tmp;
    total = 0.0f;

    /* Old data are not valid anymore */
    expansion->valid = FALSE;

    /* Loop through array */
    for(k=0; k<n; k++) {
	a = array[k];
	GROWEXP_Body;
	/* Add together the elements of the expansion */
	total = 0.0f;
	tmp = expansion;
	while(tmp != NULL && tmp->valid == TRUE){
	    total += tmp->data;
	    tmp = tmp->next;
	}
	/* Overwrite array with cumulative sum */
	array[k] = (double)total;
    }

    return(total);
}

void grow_exp(listnode *expansion, dplr_double a){
    dplr_double b,a_virtual,b_virtual,a_roundoff,b_roundoff,x,y;
    listnode *readptr, *writeptr;

    GROWEXP_Body
}
