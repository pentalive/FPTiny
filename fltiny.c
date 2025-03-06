/*
** NAME
**    fltiny.c -- interpreter for the Tiny programming language
** DESCRIPTION
**    This is an entire ANSI C89 program that interprets programs written
**    in the language Tiny.
** LICENSE TERMS
**    Copyright (C) 2006 Ron Hudson
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 3 of the License, or
**    (at your option) any later version.
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
**    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
**
***************************************************************************
** Tiny  - A small Basic like RPN language
** See tiny.html for syntax details and examples
**
** Version History
** 0.0.1  Begin keeping a verion history                        6-june-2006
**        Moved Gatherformat code to inside if not string print part
**        becuase printing a single qoute should not be a problem.
**        (By the theory that if you are printing you aren't doing
**        anything else
**
** 0.0.2  Changed by Bruce Hudson: Modified the function        4-july-2006
**        void addprogramstep(long lino, char text[80])
**        to insert lines in line number order.
**
** 0.0.21 Changed by Ron Hudson                                10-july-2006
**        Add line number reformating. Now line numbers will print only 
**        with a width that allows the largest line number to have a 
**        single leading zero.
**
** 0.0.22 Changed by Ron Hudson                                12-july-2006
**        Modified handling of parentheses so that indirect pointers
**        can now be any kind of expression instead of just a single
**        variable. [5] j  [42] (j 5+)  stores 42 in element 10 of
**        the array.
**
** 0.0.23 Changed by Ron Hudson                                 In Progress
**
**        Added a help screen...
**
**        The Debugging Facility
**        [x] Add flag to progmem structure.
**        [ ] Add commands to command parser. 
**        [ ] Show breakpoints and tracepoints in list.
**        [ ] Add breakpoint handler to execprogram().
**   
** 0.0.24 Changed by Bruce & Ron Hudson                          02-sep-2007
**
**        added \. to print '.' and
**              \a to sound bell
**              in print statments.
**        fixed filename parsing for open and save.
**
** F0.1.00  Changed by Bruce & Ron Hudson                        26-sep-2007
**
**          Forking tiny for a floating point tiny    
**          : has become the end of program
**          % has become an int() function
**          ~ returns 0 <= n < 1
**          Array indexs take the truncated value.
**          Line numbers are floating point printed to 4 decimal places
**
**
** F0.1.02  Ron Hudson                                            10-0ct-2007
**
**          Moving to beta test on floating point. Still need to change
**          code for random number generator.
**
** F00.01.03 Ron Hudson                                           10-oct-2007
**      
**           Random fixes in the debug handler. Mostly printing the line number
**           as a floating point.
**
** F00.01.04 Ron Hudson, Bruce Hudson                             11-oct-2007
**           Fix random numbers
**
*/

#define VERSION "F00.01.04" 


#include <time.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>

#define TRUE 1
#define FALSE 0

#define PUT 1
#define GET 0
#define STEPLIMIT 300
#define STACKLIMIT 30
#define ARRAYELEMENTS 999
#define PUT 1
#define GET 0
#define CMDPROMPT ">:"
#define NUMPROMPT ""
#define DEBUGPROMPT "Debug:"
#define NOBREAKPOINT 0
#define TRACEPOINT 1
#define BREAKHERE 2
#define PI 3.1415926535897932384626433832795


typedef struct statement {
  int  breakpoint;
  double lino;
  char text[80];
} STATENODE;



/* 
** Global Variables 
*/

double varz[27];                   /* Variables                         */
double ustack[STACKLIMIT];         /* $ stack                           */
double darray[ARRAYELEMENTS];      /* User Array                        */
double compstack[STACKLIMIT];      /* computational stack               */

int ustackindex;                   /* index to free stack item          */
int compstackindex;                /* index to free stack item          */
int laststep;                      /* first free step                   */
int thisstep;                      /* Line number of step running       */
int debugging;                     /* debugflag - ignore breakpoints?   */
int traceing;                      /* Traceing flag - we are traceing   */
int nowstepping;                   /* Flag, single stepping program     */

char numberformat[20];             /* Number printout format            */
char listformat[30];               /* list format                       */

STATENODE progmem[STEPLIMIT];      /* Program memory                    */

double randseed;		   /* hold the random number seed       */

/* 
** function prototypes 
*/

void setup(void);                               /* setup system */
void addprogramstep(double lino, char text[80]);
void listprogram(void);
void loadprogram(char filename[80]);
void saveprogram(char filename[20]);
void execprogram(void);
double cpop(void);                                /* Pop compstack       */
double spop(void);                                /* Pop storage stack   */
void cpush(double in);                            /* Push comp stack     */
void spush(double in);                            /* Push storage stack  */
double inputnumber(void);
void formatlisting(void);
void helpscreen(void);
double pipi(void);
void spipi(double);




int main(int argc, char *argv[]) {

  char instring[80];   /* input buffer                                    */
  char text[80];       /* Parsed input statement w/o line number          */
  char lstring[20];    /* the line number as a string                     */
  double lino;         /* Parsed line number                              */
  char c;              /* character temporary                             */
  int going;           /* flag is interpreter still going else exit       */
  int place;           /* pointer used while inputing a line              */
  int i,j,t;           /* General counter and array pointer               */
  int lsptr;           /* index to lstring                                */
  int before;          /* flag, parsing statement, flags line number      */
  double bkstep;       /* index into progmem array where a brekpoint goes */
  double lineref;      /* Reference to a line number for various reasons  */
  

  if (argc > 1) {
    
    setup();

    loadprogram(argv[1]);
    execprogram();
    /*
	printf(" Program finished press enter to close\n");
	inputnumber();
    */
    debugging = TRUE;
    exit(1);
  }

  printf("\n\nFloating Point Tiny --  Interactive Mode\n\n");
  printf("Version :%s\n",VERSION);
  printf("Tiny is provided under the \nGNU General Public License \n");
  printf("See the file COPYING for details\n\n");

  setup();


  going = TRUE;
  do {

    /* clear input buffer */
    for (i = 0; i < 80; i++) {
      instring[i] = '\0';
    }

    /* read input a string at a time */
    place = 0;

    /* print prompt */
    printf(CMDPROMPT);

    /* grab charactors until end of line or line too long */
    do {
      c = fgetc(stdin);
      instring[place] = c;
      place = place + 1;
      if (place > 80) {
	printf("\n\n-- Line too long \n");
      }
    } while( c != '\0' && c != '\n' && place < 80);
    
    /* detect and execute a command */
    if (instring[0] == '#') {

      /* bye (exit interpreter ) */
      if (tolower(instring[1]) == 'b') { 
	going = FALSE;
	printf("\n\n-- End of Session \n");
      }
      
      /* run program #r */
      if (tolower(instring[1]) == 'r') {
	execprogram();
      }

      /* #l list program */
      if(tolower(instring[1]) == 'l') {
	listprogram();
      }

      /* #s filename   Save */
      if (tolower(instring[1] == 's')) {
	/* discard '#s' and copy filename into text[] */
	i = 0;
	j = 0;
	before = FALSE;
	while (i < (int)strlen(instring)) {
	  if (instring[i] == ' ') {
	    before = TRUE;
	    /* skip the space */
	    i++;
	  }

	  if (before) {
	    if (isalnum(instring[i])|| (instring[i] == '.')) {
	      text[j] = instring[i];
	      j++;
	      text[j] = '\0';
	    }
	  }
	  i++;
	}
	saveprogram(text);

      }

      /* #n new, Clear program */
      if (tolower(instring[1] == 'n')) {
	setup();
      }


      /* #o filename  old program (load the program) */
      if (tolower(instring[1]) == 'o') {

	/* discard '#o' and copy filename into text[] */
	i = 0;
	j = 0;
	before = FALSE;
	while (i < (int)strlen(instring)) {
	  if (instring[i] == ' ') {
	    before = TRUE;
	    /* skip the space */
	    i++;
	  }


	  if (before) {
	    if (isalnum(instring[i]) || (instring[i] == '.')) {
	      text[j] = instring[i];
	      j++;
	      text[j] = '\0';
	    }
	  }
	  i++;
	}
	loadprogram(text);
      }

      /* Trace facility */
      if (tolower(instring[1]) == 'k') {

	/* get the line number */
	lineref = atof(instring+4);

	/*	for(j = 4; (j < (int) strlen(instring)); j++) {
	  if (instring[j] <= '9' && instring[j] >= '0') {
	    i = (i*10) + (instring[j] - '0');
	  }
	  }*/

	/*Find the proper element in the progmem array*/ 
	bkstep = -1;
	for(t=0; t<laststep; t++) {
	  if (progmem[t].lino == lineref) {
	    bkstep = t;
	  }
	}

	if ( bkstep < 0 && tolower(instring[3]) != 'f' ) {
	  printf("Tiny -- Can't find line number %012.4f\n",lineref);
	} else {
	  switch ( tolower(instring[3]) ) {
	  case 'n': progmem[(long)bkstep].breakpoint = NOBREAKPOINT; break;
	  case 't': progmem[(long)bkstep].breakpoint = TRACEPOINT;   break;
	  case 'b': progmem[(long)bkstep].breakpoint = BREAKHERE;    break;
	  case 'f': 
	    debugging = ! debugging;
	    if (debugging) {
	      printf("Debugging On \n");
	    } else {
	      printf("Debugging Off \n");
	    } 
	    break;
	  }
	} 
      }

      /* User asking for help? */
      if (instring[1] == '?') {
	      helpscreen();
      }

      if (isspace(instring[1])) {
      }

    }




    /* Perhaps it's a statement to store */

    if (isdigit(instring[0])) {

      /* separate statement into lino and text */
      lino = 0;
	  lsptr = 0;
      j = 0;
      before = TRUE;
      for(i = 0; i <= (int)strlen(instring);  i++) {

	/* locate break after line number */
	if ( ! isdigit( instring[i] ) && ! ( instring[i] == '.')) {
	  before = FALSE;
	}
      
	/* if still line number */
	if (before) {
	  /* collect each digit of line number */
        lstring[lsptr++] = instring[i];
		lstring[lsptr+1] = 0;
		sscanf(lstring,"%lf",&lino);

	} else {
	  /* else collect text from rest of string */
	  text[j] = instring[i];
	  j = j + 1;
	}
      }

      text[j] = '\0';   /* ensure an end of line sentinel */



      
      /* store line in program structure here... */
      addprogramstep(lino,text);

      formatlisting();
      printf(listformat,lino,text);
    }

    /*
    ** Add 'interpret no line number line here and it will
    ** only work in interactive mode. To Be added at a 
    ** later date.
    */

  } while (going);
  return 0;
}


void formatlisting(void) {

	/* fixed list format for line numbers 08.4 */
	strcpy(listformat,"%012.4lf %s");
}


void setup(void) {

  int i;


  /* initalize variables */
  for (i=0; i<27; i++) {
    varz[i] = 0;
  }
 
  /* initalize listing format */
  formatlisting();

  /* initialize number format */
  strcpy(numberformat,"%lf");

  /* initialze progrogram storage */

  for (i=0; i<STEPLIMIT; i++) {
    progmem[i].breakpoint = NOBREAKPOINT;
    progmem[i].lino = 0;
    strcpy(progmem[i].text,"");
  }

  laststep = 0;
  traceing = FALSE;
  debugging = TRUE;
}





void addprogramstep(double lino, char text[80]) {
  int i, j;

  if (lino != 0) {

    if (text[0] == '\n') {
      
      /* find and delete lino */
      i = 0;
      while ((lino != progmem[i].lino ) && (i < laststep)) i++;

      /*
      ** If found the line,
      **   then march all lines beyond up one,
      **   else i will equal laststep and the while
      **   will not run.
      */
      while (i < (laststep - 1)) {
    	progmem[i].lino = progmem[i + 1].lino;
	    strcpy(progmem[i].text, progmem[i + 1].text);
	    progmem[i].breakpoint =  progmem[i + 1].breakpoint;
	    i++;
      };

      /*
      ** Delete the last line now that all the lines have been
      ** moved forward.
      */
      progmem[i].lino = 0;
      strcpy(progmem[i].text, "");
      progmem[i].breakpoint = NOBREAKPOINT;
      laststep--;
      
    } else {
      /* Search forward */
      i=0;  
      while ((lino > progmem[i].lino) && (i < laststep)) {
    	i++;    /* bypass lower numbered lines */
      }
      if (progmem[i].lino == lino) {
	
	/*
	** Replace this line (lino is already correct, just copy in 
	** the new text.
	*/
	strcpy(progmem[i].text, text);
	progmem[i].breakpoint = NOBREAKPOINT;
      } else {
	
	if (laststep == 0) {
	  
	  /* First line of the program*/
	  progmem[0].lino = lino;
	  strcpy(progmem[0].text, text);
	  progmem[0].breakpoint = NOBREAKPOINT;
	  laststep++;
	} else {
	
	  /*
	  ** Insert this line here by saving the point to 
	  ** insert the new line and start moving the lines 
	  ** beyond to make room for the new line.
	  */
	  j = laststep;
	
	  /* Move all the lines that are beyond here */
	  while ( j > i) {
	    progmem[j].lino = progmem[j - 1].lino;
	    strcpy(progmem[j].text, progmem[j - 1].text);
	    progmem[j].breakpoint = progmem[j - 1].breakpoint;
	    j--;
	  };
	
	  /* Put in the new line */
	  progmem[i].lino = lino;
	  strcpy(progmem[i].text, text);
	  progmem[i].breakpoint = NOBREAKPOINT;
	  laststep++;
	}
      }
    }
  }
}


void listprogram(void) {
   
  int i;
  

  /* Set proper format */
  formatlisting();

  for (i=0; i < laststep; i++) {

    switch (progmem[i].breakpoint) {
    case NOBREAKPOINT: printf("  "); break;
    case BREAKHERE:    printf("* "); break;
    case TRACEPOINT:   printf("+ "); break;
    }

    printf(listformat,progmem[i].lino,progmem[i].text);
  }

  printf("\n");
}


void loadprogram(char filename[80]) {

	FILE *fp;
	char instring[80];
	char lstring[20];
	int lsptr;
	char text[80];
	int before;
	int i,j;
	double lino;
	char * eflag;

	fp = fopen(filename,"r");

	
	if (fp == NULL) {
		printf("Tiny can't open file [%s] \n",filename);

	} else {

		do {
			eflag = fgets(instring,80,fp);

			if (eflag != NULL) {

			/* separate statement into lino and text */
			lino = 0;
			lsptr = 0;
			j = 0;
			before = TRUE;
			for(i = 0; i <= (int)strlen(instring);  i++) {
	  
				/* locate break after line number */
				if ( (! isdigit( instring[i])) &&  (instring[i] != '.') ) {
					before = FALSE;
				}
	  
				/* if still line number */
				if (before) {
					/* collect each digit of line number */
					lstring[lsptr++] = instring[i];
					lstring[lsptr+1] = 0;
					sscanf(lstring,"%lf",&lino);
				} else {
					/* else collect text from rest of string */
					text[j] = instring[i];
					j++;
				}
			}
	
			text[j] = '\0';   /* ensure an end of line sentinel */

			/* printf(listformat,lino,text); */

			addprogramstep(lino,text);
	 
		}
	} while ( eflag != NULL );
    fclose(fp);
	}
}


double cpop(void){

  compstackindex--;
  return compstack[compstackindex]; 
};

double spop(void){
  ustackindex--;
  return ustack[ustackindex];
};

void cpush(double in){
  compstack[compstackindex] = in;
  compstackindex++;
};

void spush(double in){
  ustack[ustackindex] = in;
  ustackindex++;
};


double logical(double input) {
	if (input == 0) {
		return 0;
	} else {
		return 1;
	}
}


double pipirandseed;

/*
**  P I P I    -   The pipi random number generator
**  Pipi takes a fractional seed value, stored in a 
**  global pipirandseed multiply by pi then add pi.
**  take the fractional part. 
*/

double pipi(void) {
  double a,b;

  a = M_LN2 * 5.0;
  b = M_SQRT2 * 7.0;
 
  pipirandseed = pipirandseed * a + b;
  pipirandseed = fmod(pipirandseed, 1.0);
  return pipirandseed;
}


void spipi(double dx) {
	pipirandseed = dx;
}


/*
** execprogram
** 
** This executes the program that is currently loaded.
**
*/

void execprogram(void) {

  double x,y;            /* Temporary                                     */
  double exlino;         /* effective lino (@ register)                   */
  double thenumber;      /* Used to collect numeric constants             */
  char numstring[40];    /* a string in which numeric constants are built */
  int  place;            /* used in numeric constant collection           */
  char xtext[80];        /* Program text being interpreted                */
  char xchar;            /* actual char being interpreted                 */
  char xstr[2];          /* when we need a string xchar instead           */
  int i;                 /* loop indexes                                  */
  int running;           /* flag - running                                */
  int progmemstep;       /* index into progmem                            */
  int putget;            /* put get flag                                  */
  int indirect;          /* indirect for store flag                       */ 
  int numbuild;          /* building a number                             */
  int StringPrint;       /* Printing                                      */
  int BackSlash;         /* Prev Char was a backslash                     */
  int Parencomment;      /* Paren comment level                           */
  int Poundcomment;      /* Pound Comment                                 */
  int gatherformat;      /* Flag used while reading format strings        */
  int debugstopped;      /* Flag used in debugging prompt                 */
  char debugcommand[80]; /* Holds debugging input string                  */


  /* setup */
  putget       = GET;
  numbuild     = FALSE;
  StringPrint  = FALSE;
  BackSlash    = FALSE;
  indirect     = FALSE;
  Parencomment = 0;      /* not inside parens */
  Poundcomment = FALSE;  /* No Poundsign comment yet */
  gatherformat = FALSE;



  /* Get first Line Number */
  progmemstep = 0;
  exlino = progmem[progmemstep].lino;
 


  running = TRUE;
  do {

    /* locate line from @ */
    progmemstep = 0;
    while (progmem[progmemstep].lino != exlino) {
      progmemstep++;
    
      /* if past lastline then error message stop */
      if (progmemstep > laststep) {
		printf("Tiny-- Attempt to jump to %lf, line not found\n",exlino);
		running = FALSE;
		break;
      }
    }


    /* fetch line */
    strcpy(xtext, progmem[progmemstep].text);

    if (strlen(xtext) == 0) {
      printf("Tiny-- Execute past end of program\n");
      running = FALSE;
    }

    thisstep = (long) exlino;

    if (debugging && traceing) {
      printf("\033[s\033[H---------- Trace: %012.4f\033[u",exlino);
    }

    if (progmem[progmemstep].breakpoint == TRACEPOINT) {
      traceing = ! traceing;
    }

    if (progmem[progmemstep].breakpoint == BREAKHERE) {
      nowstepping = TRUE;
    }

    if ( nowstepping ) {
      printf("\033[u\033[H\033[K %012.4f %s\033[u",exlino,xtext);
      do {

	/*Assume its a short stop, get a deubgging command*/
	debugstopped = FALSE;
	printf("%s",DEBUGPROMPT);
	fgets(debugcommand,80,stdin);

	/* Debugging help */
	if (tolower(debugcommand[0]) == '?') {
		printf(" l list                  \n");
		printf(" q quit                  \n");
		printf(" n disable breakpoints   \n");
		printf(" g go to next breakpoint \n");
		printf(" b set a breakpoint      \n");
		printf(" v view a variable       \n");
		printf(" a view an array element \n");
	}

	/* Re-print current program step */
	if (tolower(debugcommand[0] == 'l')) {
	  printf("\033[u\033[H\033[K %012.4f %s\033[u",exlino,xtext);
	  debugstopped = TRUE;
	}	

	/* Return to command prompt */
	if (tolower(debugcommand[0]) == 'q') {
	  running = FALSE;
	}

	/* Disable breakpoints  */
	if (tolower(debugcommand[0]) == 'n') {
	  debugging = FALSE;
	  nowstepping = FALSE;
	}

	/* Run to next breakpoint */ 
	if (tolower(debugcommand[0]) == 'g') {
	  nowstepping = FALSE;
	}

	/* Set a breakpoint */
	if (tolower(debugcommand[0]) == 'b') {
	  debugstopped = TRUE;
	}

	/*Examine a variable*/
	if (tolower(debugcommand[0]) == 'v') {
	  double v;

	  if (debugcommand[1] <='z' && debugcommand[1] >= 'a') {

	    v = varz[tolower(debugcommand[1])-'a'];
	    printf("\033[s\033[H\033[K Variable %c = %lf \033[u",
		debugcommand[1],v);
	  }	    
	  debugstopped = TRUE;
	}


	/*Examine an array location*/
	if (tolower(debugcommand[0]) == 'a') {
	  int i,j;

	  j = 0;
	  for (i=2; i<=(int) strlen(debugcommand); i++) {
	    if (debugcommand[i] <= '9' && debugcommand[i] >= '0') {
	      j = (j * 10) + (debugcommand[i] - '0');
	    }
	    printf("\033[s\033[H\033[K Array(%d) = %lf \033[u",j,darray[j]);
	  }
	  debugstopped = TRUE;
	}



      } while (debugstopped);
    }


    /* set @ to line number of next line */
    exlino = progmem[progmemstep+1].lino;

    /* interpret line */
    for (i=0; i < (int)strlen(xtext); i++) {

      xchar = xtext[i];

      if (xchar == '#') {
		break;
      }


      if (gatherformat) {
	if (xchar == '\'') {
	  xchar = '\0';
	  gatherformat = FALSE;
	} else {
	  xstr[0] = xchar;
	  xstr[1] = '\0';
	  strcat (numberformat, xstr);
	  xchar = '\0';
	}
      }

      if (xchar == '\'') {
	/* 
	** when first we notice a single qoute, clear the numberformat
	** and begin to gather new characters in
	*/
	gatherformat = TRUE;
	strcpy(numberformat,"\0");
      }


      if (StringPrint) {
	

	if (xchar == '\\') {
	  i++;
	  xchar = xtext[i];
	  switch ( xchar ) { 
	  case 'n':  printf("\n");   break;
	  case 't':  printf("\t");   break;
	  case '\\': printf("\\");   break;
	  case 'e':  printf("\033"); break;
	  case '"':  printf("\"");   break;
	  case '\'': printf("'");    break;
	  case '.':  printf(".");    break;
	  case 'a':  printf("\a");   break;
	  default:   printf("\nTiny -- **backslash what? %c\n",xchar);
	  }

	} else {
	  if (xchar == '"') {
	    StringPrint = FALSE;
	  } else {
	    printf("%c",xchar);
	  }
	}
      } else {

	/* ':' is the "stop program " */
	if (xchar == ':') {
	  running = FALSE;
	  break;
	}


	/* 
	** Constants in programs 
	**  Here is the code that accepts constants in program
	**  statements
	*/

	if (isdigit(xchar) || (xchar == '.' )) {
	
		if ( numbuild ) {

                  place = strlen(numstring);
		  numstring[place] = xchar;
		  numstring[place+1] = '\0';
		} else {
			numbuild = TRUE;
			numstring[0] = xchar;
		        numstring[1] = '\0';
		}	
	} 
	
	/* 
	** first non digit after a number 
	*/

	if (numbuild && (! isdigit(xchar)) && (! (xchar == '.')) ) {

	  sscanf(numstring,"%lf",&thenumber);
	  cpush(thenumber);
	  numbuild = FALSE;
	}
       
	/* Get/Put Variables */
	xchar = tolower(xchar);
	if (xchar <= 'z' && 'a' <= xchar) {
	  if (putget == GET || indirect == TRUE) {

	    cpush(varz[xchar - 'a']);
	    
	  } else {
	    /* putget == Put && indirect == FALSE */
	    varz[xchar - 'a'] = cpop();
	    cpush(varz[xchar - 'a']);
	   
	  }
	}

      
	switch (xchar) {
	case '[':
	  compstackindex = 0;
	  putget = GET;
	  indirect = FALSE;
	  break;

	case ']': putget = PUT;           break;
	case '"': StringPrint = TRUE;     break;
	case '+': cpush(cpop() + cpop()); break;
	case '*': cpush(cpop() * cpop()); break;
	case '!': cpush( ! cpop());       break;
	case '_': cpush(cpop() * -1);     break;

	case '(':
	  if (putget == GET) {

	    /* does Nothing */

	  } else { /* put */

	    /* Raises indirect flag variables will be fetched */
	    indirect = TRUE;

	  }
	  break;



	case ')':
	  if (putget == GET) {

	    /* 
	    ** Replace top of stack with array pointed to by top of stack,
	    ** Lowers Indirect flag
	    */
	    indirect = FALSE;
	    cpush(darray[(long) cpop()]);
	    

	  } else { /* put */

	    /* lower indirect, stores 2nd in array(top), trash top */
	    indirect = FALSE;
	    x = cpop();
	    y = cpop();
	    darray[(long) x] = y;
	    cpush(y);

	  }
	  break;


	  /* to be replaced with an int function */
	  case '%':
	    x = cpop();
	    if (x < 0 ) {
	      cpush(ceil(x));
	    } else {
	      cpush(floor(x));
	    }
	    break; 
	  

	case '^': 
	  x = cpop();
	  y = cpop();
	  cpush((double) pow( (double) y, (double) x) );
	  break;

	case '-': 
	  x = cpop();
	  y = cpop();
	  cpush(y - x); 
	  break;

	case '/': 
	  x = cpop();
	  y = cpop();
	  if (x == 0) {
	    printf("Tiny -- %lf div by zero! Black hole forming!\n",exlino);
	    running = FALSE;
	  } else {
	    cpush(y/x);
	  }
	  break;

	case '<': 
	  x = cpop();
	  y = cpop();
	  cpush(y<x);
	  break;

	case '>': 
	  x = cpop();
	  y = cpop();
	  cpush(y>x);
	  break;

	case '=': 
	  cpush(cpop() == cpop());
	  break;


	case '&': 
	  x = logical(cpop());
	  y = logical(cpop());
	  if ((x == 1) && (y == 1)) {
	    cpush(1);
	  } else {
	    cpush(0);
	  }
	  break;


	case '|': 
		x = logical(cpop());
		y = logical(cpop());
		if ((x == 1) || (y == 1)) {
			cpush(1);
		} else {
			cpush(0);
		}
	  break;


	/* Special Variables */
	case '~':
	  if (putget == GET) {

		  cpush(pipi());

	  } else {
	    double dx;
	    x = cpop();
	    cpush(x);
	    if (x==0) {
	      dx = fmod((double) time(NULL) * M_E + M_PI, 1.0);
	    }
	    spipi(dx);
	  }
	  break;

	case '?':
	  if (putget == GET) {
	    printf("%s",NUMPROMPT);
	    x = inputnumber();
	    cpush(x);
	  } else {
	    x = cpop();
	    cpush(x);
	    printf(numberformat,x);
	  }
	  break;

	case '@':
	  if (putget == GET) {
	    cpush(exlino);
	  } else {

	    x = cpop();
	    cpush(x);
	    if (x != 0) {
	      exlino = x;
	    }
	  }
	  break;


	case '$':
	  if (putget == GET) {
	    cpush(spop());
	  } else {
	    x = cpop();
	    cpush(x);
	    spush(x);
	  }
	  break;

	}
      }
    }

    if ( compstackindex < 0) {
      printf("*** Tiny Comp Stack underflow \n");
      running = FALSE;
    }


  } while ( running );   /* execute do loop */
   
} /* execprogram */


double inputnumber(void) {

#define CR '\012'
#define BS '\000'

	double val;
	char c;
	int  i;
	char txtnumber[30];

	i = 0;
	val = 0;
	do {
		c = getchar();
		if (c == '!') {
			nowstepping = TRUE;
			debugging = TRUE;
			c = CR;
		}
		txtnumber[i]   = c;
		txtnumber[i+1] = 0;
		i++;
	} while (c != CR);
	sscanf(txtnumber,"%lf",&val);
	return val;
}


void saveprogram(char filename[20]) {

  FILE *fp;
  long i;

  /* Set listing format */
  formatlisting();

  fp = fopen(filename,"w");
  for (i = 0; i < laststep; i++) {
    fprintf(fp,listformat,progmem[i].lino,progmem[i].text);
  }
  fclose(fp);
}

void helpscreen(void) {
  printf("\033[2J\033[0;0H");
  printf("Floating Point Tiny Help                                                       \n");
  printf("Version :%s \n",VERSION);
  printf("Tiny is provided under the GNU General Public License           \n");
  printf("See the file COPYING for details                              \n\n");
  printf("=============================================================== \n");
  printf("#s filename      Save your current program                      \n");
  printf("#l               List your program                              \n");
  printf("#n               New  - Clear program memory and variables      \n");
  printf("#b               Bye  - Exit tiny                               \n");
  printf("#o filename      Old  - Load a program file into tiny           \n");
  printf("#?               Help - Print this help screen                  \n");
  printf("#r               Run  - Begin executing current program         \n");
  printf("#k t|b|n lino    Breakpoint (trace, break, none) set breakpoint \n");
  printf("=============================================================== \n");
  printf("\n\n");
}
