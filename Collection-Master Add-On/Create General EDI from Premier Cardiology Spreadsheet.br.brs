on error goto Error_Hanler
fn_setup
fnTop(program$)
do
	dim xlateFile$*256
	xlateFile$='D:\CM\Stern and Stern\Premier Cardiology crosswalk.txt'
	dim from$(0)*256
	dim to$(0)*256
	fn_readCrossWalk(xlateFile$,mat from$,mat to$)
	
	dim csvFile$*512
	if fnAsk_file1(csvFile$,env$('program_caption'), 'File to Convert','*.xlsx','Choose your New format Premier Cardiology.xlsx file.')=99 then
		goto Xit
	else
		fnCopy(csvFile$,csvFile$&'.bak')
		dim Csv_Fields$(0)*128
		dim Csv_Data$(0)*256
		csvFieldCount=Fnopen_Csv(hIn:=fnGetHandle,csvFile$,Csv_Delimiter$,Mat Csv_Fields$,Mat Csv_Data$)
		! open #hIn:= fnGetHandle: 'name='&csvFile$&'.bak',display,input
		pause
		
		
		
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
def fn_readCrossWalk(xlateFile$*256,mat from$,mat to$; ___,x,returnN)
	fnasci(xlateFile$,Mat Asci$)
	returnN=udim(mat asci$)
	mat from$(returnN)
	mat to$(returnN)
	for x=1 to returnN
		from$(x)=asci$(x)(1:pos(asci$(x),tab$)-1)
		to$(x)=asci$(x)(pos(asci$(x),tab$,-1)+1:inf)
	nex x
	fn_readCrossWalk=returnN
fnend
def fn_setup
	if ~setup then
		setup=1
		! library 'Library\clsUtil.wb': fnErase_buttons
		library 'Library\clsUtil.wb': fnAsk_file1
		library 'Library\clsUtil.wb': Fnopen_Csv
		library 'Library\clsUtil.wb': fnasci
		
		library 'S:\Core\Library.br': fnTop
		library 'S:\Core\Library.br': fnGetHandle
		library 'S:\Core\Library.br': fnFree
		library 'S:\Core\Library.br': fnCopy
		library 'S:\Core\Library.br': fnXit
		gosub Enum
		! fnErase_buttons
		

	end if
fnend
include: cm\enum\common
include: cm\err