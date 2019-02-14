! Pre Auto-Cost Spreadsheet Modifier
! Process all of them
! But program needs to process one (selected) at a time
! take date from CHECK_DATE column and add it into the COMMENT column in parenthesis
! W:\OLD_F_DRIVE\CORRESPONDING ATTY COSTS\AUTOPOST\DONE\2019\01-Janurary 2019
library 'Library\clsUtil.wb': fnErase_buttons
library 'Library\clsUtil.wb': fnAsk_file1
library 'S:\Core\Library.br': fnGetHandle
library 'S:\Core\Library.br': fnFree
library 'S:\Core\Library.br': fnCopy
library 'S:\Core\Library.br': fnXit
fnErase_buttons
do
	dim csvFile$*512
	if fnAsk_file1(csvFile$,env$('program_caption'), 'File to Convert','*.csv','Copies CHECK_DATE and append it in parenthesis to the COMMENT column')=99 then
		goto Xit
	else
		fnCopy(csvFile$,csvFile$&'.bak')
		open #hIn:= fnGetHandle: 'name='&csvFile$&'.bak',display,input
		open #hOut:=fnGetHandle: 'name='&csvFile$&',recl=1024,replace',display,output
		lineCount   =0
		colComment  =0
		colCheckDate=0
		do
			dim line$*1024
			linput #hIn: line$ eof ConversionFinis
			lineCount+=1
			dim item$(0)*512
			str2mat(line$,mat item$,',')
			if lineCount=1 then ! it's the header
				colComment  =srch(mat item$,'COMMENT' )
				colCheckDate=srch(mat item$,'CHECK_DATE')
				if colComment<=0 or colCheckDate<=0 then
					msgbox('This file does not have the appropriate column headings.Required headings are:'&CHR$(13)&'COMMENT'&CHR$(13)&'CHECK_DATE'&CHR$(13)&'This file will be skipped.')
					close #hIn:
					close #hOut:
					if fnCopy(csvFile$&'.bak',csvFile$) then
						fnFree(csvFile$&'.bak')
					end if
					goto ConversionXit
				end if
			else
				if udim(mat item$)=>colComment and udim(mat item$)=>colCheckDate and trim$(item$(colCheckDate))<>'' and pos(item$(colComment),'('&trim$(item$(colCheckDate))&')')<=0 then
					item$(colComment)=rtrm$(item$(colComment))&' ('&trim$(item$(colCheckDate))&')'
				end if
				mat2str(mat item$,line$,',')
			end if
			print #hOut: line$
		loop
	end if
	ConversionFinis: !
	close #hIn:
	close #hOut:
	msgbox('Success on '&csvFile$)
	ConversionXit: !
loop  
Xit: !
fnXit