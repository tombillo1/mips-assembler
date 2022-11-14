//  On my honor: 
// 
//  - I have not discussed the C language code in my program with 
// anyone other than my instructor or the teaching assistants 
// assigned to this course. 
// 
//  - I have not used C language code obtained from another student, 
// the Internet, or any other unauthorized source, either modified 
// or unmodified. 
// 
//  - If any C language code or documentation used in my program 
// was obtained from an authorized source, such as a text book or 
// course notes, that has been clearly noted with a proper citation 
// in the comments of my program. 
// 
//  - I have not designed this program in such a way as to defeat or 
// interfere with the normal operation of the grading code. 
// 
// Thomas Billington
// tommybillington
// Ashaz Ahmed
// ashaza
#include "parser.h"
#include "table.h"
#include "Labels.h"
#include "ParseResult.h"

/***  Add include directives for here as needed.  ***/

#include <inttypes.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

char* reg_to_binary(int val);
char* imm_to_binary(int input);
void printByte(FILE *fp, uint32_t Byte);
char* parseASM(const char* const pASM);
char* stringToBinary(char* str);
void parseFile(FILE *in, FILE *out, int pass);
void parseTokens(char** beginToken, char** endToken);
LTable preProcessLables(FILE* ptr);
void processLabels(FILE *fileName, FILE *outputFile, LTable tab);
void parseWordSeg(char** beginToken, char** endToken, FILE *outputFile);

void parseFile(FILE *in, FILE *out, int pass) {
   int textAddress = 0x00000000;
   int dataAddress = 0x00002000;
   int sizeAddresses = 1;
   bool checkData = false;
   int index;
   char *label;

   // Iterate line by line
    // Pass line to ParseResult
    char line[256];

    while (fgets(line, sizeof(line), in)) {
        index = 0;

        // Check if the line is commented
        if (line[0] == '#') {
            continue;
        }

        // Checking the labels declared in .data
        if (checkData) {
            if (line[0] == '.' && line[1] == 't') {
                checkData = false;
                continue;
            }

            // add the label to the array
            for (int i = 0; i < sizeof(line); i++) {
                if (line[i] == ':') {
                    index = i;
                    break;
                }
            }
            
            // MAYBE HAVE TO FREE
            label = calloc(index, sizeof(char));

            //Store string label name 
            strncpy(label, line, index);
            

            //Add to label table

            dataAddress++;
        }

        if (line[0] == '.' && line[1] == 'd') {
            checkData = true;
            continue;
        }

    }
}


char* parseASM(const char* const pASM) {
	
   char* holder = "";

   ParseResult* result;
   char* command;
   int index = 0;

   result = (ParseResult*) calloc(1, sizeof(ParseResult));

   result->ASMInstruction = pASM;

   for (int i = 0; i < strlen(pASM); i++) {
      if (isspace(pASM[i]) > 0) {
         index = i;
         break;
      }
   }
   command = calloc(index, sizeof(char));
   
   //make another variable using strcopy and then free command at  bottom
	//origianlly was index -1
   strncpy(command, pASM, index);
   
   char* tempCommand = calloc(index, sizeof(char));;
   strcpy(tempCommand, command);
   

   result->Mnemonic = tempCommand;
   result->Opcode = getOper(tempCommand);
   result->Funct = getFunct(tempCommand);

   char* temp = (char*) calloc(strlen(pASM) - index, sizeof(char));
   
   strncpy(temp, pASM + index + 1, strlen(pASM) - index);

   char* curr = strtok(temp, ", ");

   int count = 0;

   result->rd = 255;
   result->rs = 255;
   result->rt = 255;
   
   while (curr != NULL) {
      char* val;

      //CHECK WHAT ARGUMENT WE ARE ON
      if (count == 0) {
         val = getArg1(command);
         count++;
      }
      else if (count == 1) {
         val = getArg2(command);
         count++;
      }
      else {
         val = getArg3(command);
      }

	   if ((strcmp(command, "lw") == 0 && count == 2) || (strcmp(command, "sw") == 0 && count == 2)) 
	   {
         char* temp1 =  strtok(curr, "(");

         result->Imm = atoi(temp1);


         if(temp1 == NULL)
         {
            result->IMM = NULL;
         }
         else
         {
            result->IMM = imm_to_binary(atoi(temp1));
         }

         temp1 = strtok(NULL, ")");


         result->rsName = temp1;
         result->rs = getValue(temp1);
         if(temp1 == NULL)
         {
            result->RS = NULL;
         }
         else
         {
            result->RS = reg_to_binary(result->rs);
         }

         curr = NULL;
         continue;
      }

      //CHECK TO SEE WHAT ARGUMENT WE ARE CHANGING
      if (strcmp(val, "rd") == 0) 
      {
         result->rdName = curr;
		   result->rd = getValue(curr);
		 
		   if(curr == NULL)
		   {
			   result->RD = NULL;
	      }
	      else
	      {
			   result->RD = reg_to_binary(result->rd);
	      }
      }
      else if (strcmp(val, "rs") == 0) {
         result->rsName = curr;
         result->rs = getValue(curr);
         if(curr == NULL)
		   {
			   result->RS = NULL;
	      }
	      else
	      {
			   result->RS = reg_to_binary(result->rs);
	      }
      }
      else if (strcmp(val, "rt") == 0) 
      {
         result->rtName = curr;
         result->rt = getValue(curr);
         if(curr == NULL)
		   {
			   result->RT = NULL;
	      }
	      else
	      {
			   result->RT = reg_to_binary(result->rt);
	      }
      }
      else if (strcmp(val, "immediate") == 0) {
         result->Imm = atoi(curr);
         if(curr == NULL)
		   {
			   result->IMM = NULL;
	      }
	      else
	      {
			   result->IMM = imm_to_binary(atoi(curr));
	      }

      }

      curr = strtok(NULL, ", ");
   }


	// comparisons for different instructions
   // lui has already been done above


   if(strcmp(result->Mnemonic, "lw") == 0 || strcmp(result->Mnemonic, "sw") == 0 || strcmp(result->Mnemonic, "addi") == 0 || strcmp(result->Mnemonic, "addiu") == 0 || strcmp(result->Mnemonic, "andi") == 0 || strcmp(result->Mnemonic, "slti") == 0)   // for sw and lw instructions
   {
      strcat(holder, result->Opcode);
      strcat(holder, result->RT);
      strcat(holder, result->RS);
      strcat(holder, result->IMM);
   }
   else if(strcmp(result->Mnemonic, "add") == 0 || strcmp(result->Mnemonic, "addu") == 0 || strcmp(result->Mnemonic, "and") == 0 || strcmp(result->Mnemonic, "nor") == 0 || strcmp(result->Mnemonic, "slt") == 0 || strcmp(result->Mnemonic, "mul") == 0 || strcmp(result->Mnemonic, "sub") == 0 || strcmp(result->Mnemonic, "srav") == 0 || strcmp(result->Mnemonic, "sra") == 0) // for R type instructions
   {
      strcat(holder, result->Opcode);
      strcat(holder, result->RD);
      strcat(holder, result->RS);
      strcat(holder, result->RT);
   }
   else if(strcmp(result->Mnemonic, "sll") == 0)
   {
      strcat(holder, result->Opcode);
      strcat(holder, result->RD);
      strcat(holder, result->RT);
      strcat(holder, result->IMM);
   }
   else if(strcmp(result->Mnemonic, "beq") == 0 || strcmp(result->Mnemonic, "bne") == 0 || strcmp(result->Mnemonic, "blez") == 0)
   {
      strcat(holder, result->Opcode);
      strcat(holder, result->RS);
      strcat(holder, result->RT);
      strcat(holder, result->IMM);
   }
   else if(strcmp(result->Mnemonic, "mult") == 0)
   {
      strcat(holder, result->Opcode);
      strcat(holder, result->RS);
      strcat(holder, result->RT);
   }
   //WE NEED TO IMPLEMENT SOMETHING FOR J AND ALSO FOR THE LABEL STUFF
   else if (strcmp(command, "lui") == 0) 
   {
      result->rs = 0;
      result->RS = "00000\0";

      strcat(holder, result->Opcode);
      strcat(holder, result->RS);
      strcat(holder, result->RT);
      strcat(holder, result->IMM);
   }
   else if (strcmp(command, "bgtz") == 0) 
   {
      result->rs = 0;
      result->RT = "00000\0";

      strcat(holder, result->Opcode);
      strcat(holder, result->RS);
      strcat(holder, result->RT);
      strcat(holder, result->IMM);
   }
   else if(strcmp(command, "nop") == 0)
   {
      result->rs = 0;
      result->RS = "00000\0";

      result->rd = 0;
      result->RD = "00000\0";

      result->rt = 0;
      result->RT = "00000\0";

      strcat(holder, result->Opcode);
      strcat(holder, result->RD);
      strcat(holder, result->RS);
      strcat(holder, result->RT);
   }
   else if(strcmp(command, "syscall") == 0 )
   {
      result->rs = 0;
      result->RS = "00000\0";

      result->rd = 0;
      result->RD = "00000\0";

      result->rt = 0;
      result->RT = "00000\0";

      strcat(holder, result->RD);
      strcat(holder, result->RS);
      strcat(holder, result->RT);
      strcat(holder, result->Opcode);
   }

   return holder;
}

//converts a number to a binary string
char* reg_to_binary(int val)
{
	int num = sizeof(int) * 8;
    char* str = malloc(num + 1);
	int count = 0;
	  
    for(int i = 4; i >= 0; i--)
    {
		int j = val >> i;
		if(j & 1)
		{
			str[count] = '1';
		}
		else
		{
		    str[count] = '0'; 
		}
		count += 1;
    }
    str[num] = '\0';
    return str;
}

char* imm_to_binary(int input)
{
    unsigned int val = (unsigned)input;
    //counter variable
    int count = 0;
    int arr[15]; //holder array

    //string array
    int num = sizeof(int) * 16;
    char* str = malloc(num + 1);

    //loops to get binary number
    while(val > 0)
    {
        arr[count] = val % 2;
        val = val/2;
        count += 1;
    }
    //reverses the binary number and stores in string
    int count2 = 0;
    for(int i = 15; i >= 0; i--)
    {
        str[count2] = arr[i] + '0';
        if(str[count2] != '0' || str[count2] != '1')
        {
		    str[count2] = '0';
	    }
        count2 += 1;
    }
    int c = 0;
    while(str[c] != '1' && c != 15)
    {
        str[c] = '0';
        c += 1;
    }
   
    return str;
}

void printByte(FILE *fp, uint32_t Byte)
{
    uint32_t Mask = 0x80000000;

    for (int bit = 32; bit > 0; bit--)
    {
        fprintf(fp, "%c", ((Byte & Mask) == 0 ? '0' : '1'));
        Mask = Mask >> 1;
    }
}

char* stringToBinary(char* str) 
{
	if(str == NULL){
		return 0;
	}
	size_t l = strlen(str);
	char* bin = malloc(l *8 +1);
	bin[0] = '\0';
	for(size_t i = 0; i < l; i++)
	{
		char c = str[i];
		for(int j = 7; j >= 0; --j)
		{
			if(c & (1 << j))
			{
				strcat(bin, "1");
			}
			else{
				strcat(bin, "0");
			}
		}
	}
	return bin;
}

//parses the token and ignores white space
void parseTokens(char** beginToken, char** endToken)
{
   //checks to make sure the tokens are made
   if(*beginToken == NULL || *endToken == NULL || beginToken == NULL || endToken == NULL)
   {
      return 0;
   }

   //goes until no whitespace
   while (**beginToken != '\0')
   {
      if(isspace(**beginToken))
      {
         (*beginToken) += 1;
      }
      else
      {
         break;
      }
   }

   //if is a blank row than set end to front
   if(**beginToken == '\0')
   {
      *endToken = *beginToken;
   }

   //gets the token and checks to see if there is a label or a .word, .asciiz, etc section 
   *endToken = *beginToken + 1;
   if (**beginToken == '.')
   {
      while(!isspace (**endToken))
      {
         (*endToken) += 1;
      }
   }
   else
   {
      while(**endToken != ':' && **endToken != '\0')
      {
         (*endToken) += 1;
      }
   }
   
}

//loops through and preprocesses the labels into the table
//first pass
LTable preProcessLables(FILE* ptr)
{
   LTable tab;
   tableDef(&tab);

   char* startToken;
   char* endToken;
   int addr = 0;
   bool inDataSegment = false;
   int dataAddr = 2000;

   char instruction[256];

   while(fgets(instruction, 256, ptr))
   {
      //checks for comments either at the start or after instructions
      if(*instruction == '#')
      {
         continue;
      }
      else if (*instruction == '.' && *instruction + 1 == 'd') {
         inDataSegment = true;
      }
      else if (*instruction == '.' && *instruction + 1 == 't') {
         inDataSegment = false;
      }
      else{
         strtok(instruction, "#");
      }

      startToken = instruction;
      endToken = startToken;
      parseTokens(&startToken, &endToken);

      if(*endToken == ':')
      {
         *endToken = '\0';
         if(getLab(&tab, startToken) == 0 && inDataSegment)
         {
            addLab(&tab, startToken, dataAddr);
         }
         else if (getLab(&tab, startToken) == 0) {
            addLab(&tab, startToken, addr);
         }
      }

      if (inDataSegment) {
         dataAddr += 4;
      }
      else {
         addr += 4;
      }
   }
   return tab;
}

//2nd pass which should handle the .data and .text segments as well as all instructions and labels
void processLabels(FILE *fileName, FILE *outputFile, LTable tab)
{
   char *startToken;
   char *endToken;
   bool inDataSegment = false;

   char instruction[256];

   while (fgets(instruction, 256, fileName))
   {
      // checks for comments either at the start or after instructions
      if (*instruction == '#')
      {
         continue;
      }
      else if (*instruction == '.' && *instruction + 1 == 'd')
      {
         inDataSegment = true;
      }
      else if (*instruction == '.' && *instruction + 1 == 't')
      {
         inDataSegment = false;
      }
      else
      {
         strtok(instruction, "#");
      }

      startToken = instruction;
      endToken = startToken;
      parseTokens(&startToken, &endToken);

      if (endToken == ':' && !inDataSegment) {
         continue;
      }
      else if (endToken == ':') {
         startToken = endToken +1;
         endToken = startToken;
         parseTokens(&startToken, &endToken);

         //in a .word segment
         if(startToken+1 == 'w')
         {
            startToken = endToken +1;
            endToken = startToken;
            parseWordSeg(&startToken, &endToken, &outputFile);
         }
         //in a .asciiz segment
         else if(startToken+1 == 'a')
         {
            //THIS DOES NOT WORK YET
            startToken = endToken +1;
            endToken = startToken;
            parseTokens(&startToken, &endToken);
            char* temp = stringToBinary(startToken);
            fprintf(outputFile, temp);
         }
      }

      char* result = parseASM(startToken, tab);

      //Add to file

   }
}

//parses the .word seg and deals with arrays, len of word, etc.
void parseWordSeg(char** beginToken, char** endToken, FILE *outputFile)
{
  //checks to make sure the tokens are made
  if(*beginToken == NULL || *endToken == NULL || beginToken == NULL || endToken == NULL)
  {
    return 0;
  }

  //goes until no whitespace
  while (**beginToken != '\0')
  {
    if(isspace(**beginToken))
    {
      (*beginToken) += 1;
    }
    else
    {
      break;
    }
  }

  //if is a blank row than set end to front
  if(**beginToken == '\0')
  {
    *endToken = *beginToken;
  }
  
  //gets the token and checks to see if there is a label or a .word, .asciiz, etc section 
  
  int holder = 0;
  while(**endToken != '\0')
  {
    if(isspace (**endToken))
    {
      (*endToken) += 1;
    }
    else if(**endToken == ',')
    {
      fprintf(outputFile, holder);
      holder = 0;
      (*endToken) += 1;
    }
    else
    {
      
      holder *= 10;
      int temp = atoi(*endToken);
      holder += temp;
      
      int count = 0;
      int div = holder;
      do {
        div /= 10;
        ++count;
      } while (div != 0);
      
      (*endToken) += count;
    }
  }
  //printf("Val: %d", holder);
  fprintf(outputFile, holder);
}

//gets the next token in the  line
void parseTokens(char** beginToken, char** endToken)
{
   //checks to make sure the tokens are made
   if(*beginToken == NULL || *endToken == NULL || beginToken == NULL || endToken == NULL)
   {
      return 0;
   }

   //goes until no whitespace
   while (**beginToken != '\0')
   {
      if(isspace(**beginToken))
      {
         (*beginToken) += 1;
      }
      else
      {
         break;
      }
   }

   //if is a blank row than set end to front
   if(**beginToken == '\0')
   {
      *endToken = *beginToken;
   }

   //gets the token and checks to see if there is a label or a .word, .asciiz, etc section 
   *endToken = *beginToken + 1;
   if (**beginToken == '.')
   {
      while(!isspace (**endToken))
      {
         (*endToken) += 1;
      }
   }
   else
   {
      while(**endToken != ':' && **endToken != '\0')
      {
         (*endToken) += 1;
      }
   }
   
}
