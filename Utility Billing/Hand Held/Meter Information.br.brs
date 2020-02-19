00020   fn_setup
00070   fntop(program$)
00130   if fnhand_held_device$<>'Itron FC300' and fnhand_held_device$(1:6)<>'[Ask]' and fnhand_held_device$(1:6)<>'Aclara' and fnhand_held_device$<>'Master Meter' then 
00140     dim msg_text$(2)*256
00150     msg_text$(1)='The '&env$('program_caption')&' file is not necessary'
00160     msg_text$(2)="for your hand held device type."
00170     fnmsgbox(mat msg_text$, response$,'',64)
00190   end if 
00241   ! ! if env$('acsdeveloper')<>'' then 
00242   ! !   exec 'free "'&"[Q]\UBmstr\Meter.h[cno]"&'"'
00243   ! !   exec 'free "'&"[Q]\UBmstr\Meter_Idx.h[cno]"&'"'
00244   ! !   fn_open_meter
00245   ! !   fn_CsvToMeterInformation 
00246   ! !   stop
00247   ! ! end if
00260   fnHamsterFio("UB Meter Info")
00280   goto XIT
00290 XIT: fnxit
10000 !
12000 def fn_setup
12020   if ~setup then
12040     setup=1
12060     library 'S:\Core\Library': fntop,fnxit,fnerror,fnmsgbox,fngethandle,fnhand_held_device$,fnget_services
12080     library 'S:\Core\Library': fnH2Init,fnH2AddText,fnH2AddComboA,fnH2AddComboF,fnHamster2
12080     library 'S:\Core\Library': fnHamsterFio
12100     on error goto Ertn
12120   end if
12140 fnend 
14000 ! def library fnopen_meter
14020 !   fn_setup
14040 !   fnopen_meter=fn_open_meter
14060 ! fnend 
16000 ! def fn_open_meter
16020 !   fn_open_file
16040 !   close #open_file(1):
16060 !   fn_open_meter=fn_open_file
16080 ! fnend 
18000 ! def fn_open_file
18020 !   open_file_count=0 ! this value is used in the close_file sub routine
18040 !   mat open_file(1)
18060 !   open #open_file(open_file_count+=1):=fngethandle: "Name=[Q]\UBmstr\Meter.h[cno],Version=1,KFName=[Q]\UBmstr\Meter_Idx.h[cno],Use,RecL=384,KPs=1/11,KLn=10/2,Shr",internal,outIn,keyed 
18080 !   fn_open_file=open_file(1)
18100 ! fnend 
20700 ! <Updateable Region: ERTN>
20710 ERTN: fnerror(program$,err,line,act$,"xit")
20720   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20730   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20740   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20750 ERTN_EXEC_ACT: execute act$ : goto ERTN
20760 ! /region
32000 def fn_CsvToMeterInformation
32020   colP=16
32040   dim line$*1024,item$(0)*256
32060   open #hIn:=fngethandle: 'name='&env$('at')&'C:\ACS\(Client_Files)\Purdy\Purdy MO Installation List 20171015 Final All.txt',d,input
32080   open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
32100   linput #hIn: line$ eof CtmiEof ! consume headers
32120   do
32140     linput #hIn: line$ eof CtmiEof
32160     str2mat(line$,mat item$, chr$(9))
32180     fn_isKeyValid(item$(1),hCustomer) ! pr item$(1),item$(colP),fn_isKeyValid(item$(1),hCustomer)
32200     if item$(colP)='1 inch' then 
32220       MeterType=1
32240     else if item$(colP)='2 inch T-10' then 
32260       MeterType=21
32280     else if item$(colP)='1.5 inch' then 
32300       MeterType=15
32320     else if item$(colP)='2 inch Turbine' then 
32340       MeterType=2
32360     else if item$(colP)='3 inch' then 
32380       MeterType=3
32400     else if item$(colP)='4 inch' then 
32420       MeterType=4
32440     else if item$(colP)='6 inch' then 
32460       MeterType=6
32480     else
32500       if item$(colP)<>'5/8x3/4' then pr item$(colP) : pause
32520       MeterType=5
32540     end if
32550     write #open_file(1),using 'form pos 1,c 10,c 2,C 17,c 17,c 12,c 20,n 5': trim$(item$(1)),'WA','','','','',MeterType
32560   loop
32580   CtmiEof: !
32600 fnend
34000 def fn_isKeyValid(&key$,hFile)
34020   ikvReturn=0
34040   key$=lpad$(trim$(key$),kln(hFile))
34060   read #hFile,key=key$: nokey IkvTryRpad
34080   ikvReturn=1
34100   goto IkvFinis
34120   IkvTryRpad: !
34140     key$=rpad$(trim$(key$),kln(hFile))
34160     read #hFile,key=key$: nokey IkvNoKey
34180     ikvReturn=2
34200   goto IkvFinis
34220   IkvNoKey: !
34240     ikvReturn=0 : pr 'key failed: '&key$ ! pause
34260   goto IkvFinis
34280   IkvFinis: !
34300   fn_isKeyValid=ikvReturn
34320 fnend
      
