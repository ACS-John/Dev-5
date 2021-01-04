def library fnReplaceLFwithSpace$*256(csvFile$*256)
	if ~setup then fn_setup
	fnReplaceLFwithSpace$=fn_replaceLFwithSpace$(csvFile$)
fnend
def fn_replaceLFwithSpace$*256(csvFile$*256; ___,line$*1024,outFile$*300, _
			line2$*1, _
			line500$*500)
	outFile$=csvFile$&env$('unique_computer_id')&'.tmp'
	open #hIn=fnH:  'name='&csvFile$&',RecL=500,EoL=None',external,input
	open #hOut=fnH:  'name='&outFile$&',RecL=2048,EoL=None,Replace',external,output
	do
		rea #hIn,using 'form pos 1,c 500': line500$ eof RlsEoF
		line$=line500$
		lineLen=len(line$)
		if fn_lastChr$(line$)=cr$ then
			pr 'last character is [cr]  add logic'
			pause
			rea #hIn: line2$
			line$&=line2$
		end if
		line$=srep$(line$,crlf$,'¼½')
		line$=srep$(line$,lf$,' ')
		line$=srep$(line$,'¼½',crlf$)
		pr #hOut: line$;
	loop
	RlsEoF: !
	close #hIn:
	close #hOut:
	fnCopy(outFile$,csvFile$)
	fnFree(outFile$)
	fn_replaceLFwithSpace$=csvFile$
fnend
def fn_chunkOpen(filename$*80,&vrec,&chunk,&rpos,&file_size,&trec,&head)
	dim wb_results(1)
	fn_readBrIntro(filename$,mat wb_results)
	vrec=wb_results(4)+1
	trec=wb_results(4) 
	rpos=wb_results(5)+1
	head=wb_results(5)
	file_size=wb_results(5)+wb_results(1)*vrec
	if wb_results(7)<0 then goto L4744
	max_rec=2**15-1
	chunk=int(max_rec/vrec)*vrec 
	if chunk>file_size then chunk=int(file_size/vrec)*vrec
	if chunk+head>file_size then 
		chunk=file_size-head
	end if
	open #hsuper=fnH: "name="&trim$(filename$)&",recl="&str$(chunk)&",shr",external,input,relative ioerr L4744 ! outin
	fn_chunkOpen=hsuper
	goto L4746
	L4744: !
		fn_chunkOpen=-err
	goto L4746
	L4746: !
fnend
def fn_readBrIntro(filname$*80,mat results; ___,hTmp)
	hTmp=fnH
	mat results(7)=(0) 
	! 1 = LastRec 
	! 2 = LastSort 
	! 3 = FileType 
	! 4 = RecLen 
	! 5 = Head 
	! 6 = Total # Bytes 
	! 7 = Estimated LastRec
	open #hTmp: "Name="&filname$&",RecL=16,shr",external,input ioerr WbhOpenErr
	read #hTmp,using 'form pos 1,b 4,b 4,b 1,b 2,b 2',release: mat results(1:5) ioerr ignore
	close #hTmp: ioerr ignore
	open #hTmp: 'Name='&filname$&',RecL=1,Shr',external,input ioerr WbhXit
	results(6)=lrec(hTmp) 
	if results(6)<0 then results(6)+=2**32
	results(7)=(results(6)-results(5))/(results(4)+1)
	close #hTmp: ioerr ignore
	goto WbhXit
	WbhOpenErr: ! r:
		results(7)=-err
	goto WbhXit ! /r
	WbhXit: ! 
	fn_readBrIntro=results(7)
fnend 
def library fnRemoveExcessCRLF$*256(csvFile$*256; minColCount, ___, _
						return$*256,hIn,hOut,line$*1024,delim$,lineCount, _
						itemsOnLine,lineCountIn,lineCountOut,quoteCount, _
						quoteCountIsEven)
	! if mode=1 then
	if ~setup then fn_setup
	csvFile$=fn_replaceLFwithSpace$(csvFile$)
	lineCount=0
	open #hIn=fnH:  'name=[at]'&csvFile$,display,input
include: filenamesPushMixedCase
	open #hOut=fnH: 'name=[at]'&csvFile$&'-fixedCrLf, _
														recl=2048,replace',display,output
include: filenamesPopUpperCase

	do
		linput #hIn: line$ eof Recrlf_EoF
		lineCountIn+=1
		lineLen=len(rtrm$(line$))
		if lineCountIn=1 and delim$='' and pos(line$,chr$(9))>0 then delim$=chr$(9)
		dim re_item$(0)*2048
		str2mat(line$&' ',mat re_item$,delim$)
		itemsOnLine=udim(mat re_item$)
		re_item$(itemsOnLine)=trim$(re_item$(itemsOnLine))
		! itemsOnLine+=fn_itemCount(line$,delim$)

		! quoteCount=fnChrCount(line$,'"')
		! quoteCountIsEven=0 : if int(quoteCount/2)=quoteCount/2 then quoteCountIsEven=1
		! pr 'quoteCountIsEven=';quoteCountIsEven
		! pr 'line$=';line$
		! if quoteCountIsEven and (fn_lastChr$(line$)='"'  then
		! 	pr 'quoteCountIsEven - printing the line with CRLF'
		! 	pr #hOut: line$
		! else
		! 	pr 'quoteCountIs not Even - printing the line with no ending'
		! 	pr #hOut: line$;
		! end if
		! pause
		! priorQuoteCountIsEven=quoteCountIsEven
		
		pr unhex$(line$(lineLen-1:lineLen))
		if int(lineCountIn/20)=lineCountIn/20 then pause
		
		if (re_item$(itemsOnLine)(1:1)='"' and fn_lastChr$(re_item$(itemsOnLine))<>'"') _
		or fn_haventMetMinColCountYet then
			print #hOut: line$&'    ';
			fn_sendToOutput(line$&'    '&'^')
		else
			fn_sendToOutput(line$)
			lineCountOut+=1
		end if
	loop
	
	dim output$*4096
	
	Recrlf_EoF: !
	return$=file$(hOut)
	if env$("ACSDeveloper")<>'' then
		pr 'from the source file:'
		pr file$(hIn)
		pr 'i created an output file without CRLF in the middle of it''s strings (see return$)'
		pr file$(hOut)
	end if
	close #hIn:
	close #hOut:
	fnpause
	fnRemoveExcessCRLF$=return$
fnend
def fn_sendToOutput(line$*4096) !  local only - used in debugging
	! output$ is the last line out... 
	!   if it ends with a ^ than we'll append it next time (last time was printed with a semi)
	if fn_lastChr$(output$)='^' then 
		print #hOut: line$;
		output$(len(output$):len(output$))=''
		output$&=line$(1:len(line$)-1)
	else
		print #hOut: line$
		output$=line$
	end if
fnend
def fn_haventMetMinColCountYet(; ___,returnN) !  requires local: itemsOnLine,minColCount
	if minColCount and itemsOnLine<minColCount then
		returnN=1
	else
		returnN=0
	end if
	fn_haventMetMinColCountYet=returnN
fnend
def fn_lastChr$*1(strin$*2048)
	fn_lastChr$=strin$(len(rtrm$(strin$)):len(rtrm$(strin$)))
fnend

def fn_setup
	on error goto Ertn
	if ~setup then
		setup=1
		autoLibrary
		gosub Enum

	end if
fnend
include: cm\enum\common
include: cm\enum\forw
include: cm\err
