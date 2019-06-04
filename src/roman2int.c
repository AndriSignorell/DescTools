
// from gtools



#include <R.h>

void convert(
	     char**  letters,
	     int*  nchar,
	     int*  values
	     )
{
  if(*nchar<1) return;

  for(int i=0; i<*nchar; i++)
    {
      if(letters[0][i]== 'I')
	values[i]=1;
      else if(letters[0][i]== 'V')
	values[i]=5;
      else if(letters[0][i]== 'X')
	values[i]=10;
      else if(letters[0][i]== 'L')
	values[i]=50;
      else if(letters[0][i]== 'C')
	values[i]=100;
      else if(letters[0][i]== 'D')
	values[i]=500;
      else if(letters[0][i]== 'M')
	values[i]=1000;
      else error("Invalid roman numeral '%c'", letters[0][i]);
    }
}

void roman2int(char** str,
               int*  nchar,
               int*  retval)
{
  if (*nchar < 1)
    {
      *retval = NA_INTEGER;
      return;
    }

  int* values = (int*) R_alloc(*nchar, sizeof(int));
  convert(str, nchar, values);

  int total=0;
  if (*nchar > 1)
    {
      for(int n=0; n<*nchar-1; n++)
	{
	  if(values[n]<values[n+1])
	    {
	      total-=values[n];
	    }
	  else
	    {
	      total+=values[n];
	    }
	}
    }
  total += values[*nchar-1];

  retval[0] = total;
}
