#include "nfmenu.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
fmenu *Start, *End;

/* linked list code
	ammeraal, l,
	programs and data structures in c, second edition,
		based on ansi c and c++,
	john wiley & sons, chichester
	1992.
	272 pp

	pages 111-112
*/

void *getmemory(int n)
{
	void *p=malloc(n);
	if(p == NULL){
		fprintf(stderr,"not enough memory\n");
		exit(1);
	}
	return p;
}

fmenu *getnode(void)
{
	return (fmenu *)getmemory(sizeof (fmenu));
}

void deletenode(fmenu *p)
{
	fmenu *q;
	free(p->str);
	q = p->next;
	if(q == End)
		End = p;
	else
		*p = *q;
	free(q);
}

void insertnode(fmenu *p, float xl, float yl, float xh, float yh,
	char *str, int action, int lstrmx, int type, int line, int fsize,
	int nsamp, char *kstnm, char *kcmpnm, char *datetime,
	int page, int used, float dist, float az, float baz)
{
	fmenu *q;
fprintf(stderr,"insertnode str %s kstnm %s kcmpnm %s datetime %s %d\n",str,kstnm,kcmpnm,datetime,(int)strlen(str));
	q = getnode();
fprintf(stderr,"01\n");
	if(p == End)
		End = q;
	else
		*q = *p;
	p->next = q;
	p->xl = xl;
	p->yl = yl;
	p->xh = xh;
	p->yh = yh;
	p->str = (char *)getmemory(strlen(str)+1);
	strcpy(p->str, str);
	p->action = action;
	p->lstrmx = lstrmx;
	p->type   = type  ;
	p->line   = line  ;
	p->fsize  = fsize ;
fprintf(stderr,"12 kstnm %s strlen(kstnm) %d %d\n",kstnm,(int)strlen(kstnm),
(int)strlen(p->kstnm));
	strncpy(p->kstnm, kstnm,8);
	(p->kstnm)[8] = '\0';
fprintf(stderr,"13 kcmpnm %s strlen(kcmpnm) %d\n",kcmpnm,(int)strlen(kcmpnm));
	strncpy(p->kcmpnm, kcmpnm,8);
	(p->kcmpnm)[8] = '\0';
fprintf(stderr,"14\n");
datetime[23]='\0';
	strcpy(p->datetime, datetime);
fprintf(stderr,"15\n");
	p->nsamp  = nsamp ;
	p->page   = page  ;
	p->used   = used  ;
	p->dist = dist;
	p->az = az;
	p->baz = baz;
fprintf(stderr,"insertnode str %s kstnm %s kcmpnm %s datetime %s\n",str,kstnm,kcmpnm,datetime);
}


void appendnode(float xl, float yl, float xh, float yh,
	char *str, int action, int lstrmx, int type, int line, int fsize,
	int nsamp, char *kstnm, char *kcmpnm, char *datetime,
	int page, int used, float dist, float az, float baz)
{
fprintf(stderr,"appendnode str %s kstnm %s kcmpnm %s datetime %s %d\n",str,kstnm,kcmpnm,datetime,(int)strlen(str));
	fmenu *p=End;
	End = getnode();
	p->next = End;
	p->xl = xl;
	p->yl = yl;
	p->xh = xh;
	p->yh = yh;
	p->str = (char *)getmemory(strlen(str)+1);
	strcpy(p->str, str);
	p->action = action;
	p->lstrmx = lstrmx;
	p->type   = type  ;
	p->line   = line  ;
	p->fsize  = fsize ;
	p->nsamp  = nsamp ;
	strcpy(p->kstnm, kstnm);
	strcpy(p->kcmpnm, kcmpnm);
	strcpy(p->datetime, datetime);
	p->page  = page ;
	p->used   = used  ;
	p->dist = dist;
	p->az = az;
	p->baz = baz;
fprintf(stderr,"appendnode str %s kstnm %s kcmpnm %s datetime %s\n",str,kstnm,kcmpnm,datetime);
}



