/*
** bison doesn't put these into ptaz.h
*/

extern	int		tazyyparse(void);
extern	const char	*tazyyfile;
extern	int		tazyylinenum;

/*
** flex doesn't put this into ltaz.h (it doesn't even create ltaz.h)
*/

extern	int		tazyylex(void);
extern	FILE		*tazyyin;

/*
** stdio.h defines this only with some options, not with others
*/

extern	int		fileno(FILE *);
