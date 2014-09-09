#include <stdio.h>

int main()
	{
	int blanks, tabs, newlines;
	blanks = 0;
	tabs = 0;
	newlines = 0;

	int c;
	while ((c=getchar()) != EOF)
		{
		if (c == ' ' || c == '\n' || c == '\t')
			{
			++blanks;
			if (c == '\n')
				++newlines;
			if (c == '\t')
				++tabs;
			}
		}
	printf("Blanks: %d\tTabs: %d\tNewlines: %d\n",blanks,tabs,newlines);
	}
