12000 def fn_setup
12020   if ~setup then
12040     setup=1
12060     library 'S:\Core\Library': fnAddOneC,fnOpenFile,fnAddOneN,fnHamster,fngethandle,fnerror
12080     dim form$(0)*256
12120     dim hfLabel$(0)*128
12480   end if
12900 fnend
22000 def library fnHamsterFio(fileid$*64)
22020   if ~setup then let fn_setup
22040   dim DefaultFileLayoutPath$*256
22060   DefaultFileLayoutPath$="S:\Core\FileIO\Layout\"
22080   DefaultFileLayoutExtension$=".fio"
22100   dim hfData$(0)*2048
22120   dim hfDataN(0)
22140   dim hfDataAll$(0)*2048
22160   hFile=fn_open(fileid$,mat hfData$,mat hfDataN,mat form$)
22180   if hFile then
22200     fn_hfLayoutRead(DefaultFileLayoutPath$&fileid$&DefaultFileLayoutExtension$,mat hfDataAll$,mat hfLabel$,mat hfFieldType$,mat hfStorageLen,mat hfMask,mat hfFieldLen)
22220     fnHamster(fileid$,mat hfLabel$,mat hfFieldLen,hFile,mat hfDataAll$,mat hfFieldType$,mat hfStorageLen,mat hfMask,mat startingPosition,mat comboBox$)
22240   end if
22260 fnend
24000 def fn_hfLayoutRead(hfLayoutFilename$*256,mat hfDataAll$,mat hfLabel$,mat hfFieldType$,mat hfStorageLen,mat hfMask,mat hfFieldLen)
24020   dim line$*1024,hfItem$(0)*1024
24040   open #hLay:=fngethandle: 'name='&hfLayoutFilename$,d,i
24060   past_header=0
24080   hfItem=0
24100   mat hfDataAll$(0) : mat hfLabel$(0) : mat hfFieldType$(0) : mat hfStorageLen(0) : mat hfMask(0) : mat hfFieldLen(0) 
24112   dim comboBox$(0,9)*256 ! comboBox$(0,8)*256<-worked most of the time.    comboBox$(60,64)*256
24114   mat comboBox$(0,udim(mat comboBox$,2)) ! mat comboBox$=('') ! 
24120   do
24140     linput #hLay: line$ 
24160     if line$(1:6)='======' then past_header=1
24180   loop until past_header
24200   do
24220     linput #hLay: line$ eof hfEofhLay
24240     if trim$(line$)(1:1)<>'!' and trim$(line$)<>'' and trim$(line$)(1:1)<>'#' then
24250       ! r: basic stuff like file layout and text
24260       str2mat(line$,mat hfItem$,',')
24280       hfItem+=1
24300       fnAddOneC(mat hfDataAll$,'')
24320       fnAddOneC(mat hfLabel$,trim$(hfItem$(2)))
24340       hfItem$(3)=trim$(hfItem$(3))
24360       posSpace=pos(hfItem$(3),' ')
24380       fnAddOneC(mat hfFieldType$,uprc$(trim$(hfItem$(3)(1:posSpace-1))))
24400       hfItem$(3)(1:posSpace-1)=''
24420       hfItem$(3)=trim$(hfItem$(3))
24440       fnAddOneN(mat hfStorageLen,val(hfItem$(3)))
24460       fnAddOneN(mat hfMask,0) ! accumulated last
24480       tmp=int(hfStorageLen(hfItem))
24500       if uprc$(hfFieldType$(hfItem))='PD' then tmp=tmp*2-1
24520       fnAddOneN(mat hfFieldLen,tmp)
24540       if pos(lwrc$(hfItem$(4)),' required=true')>0 then hfMask(hfitem)+=1000
24560       do while pos(hfItem$(4),'= ')>0
24580         hfItem$(4)=srep$(hfItem$(4),'= ','=')
24600       loop 
24620       hfItem$(4)=trim$(hfItem$(4))
24640       if hfItem$(4)(1:1)='!' then hfItem$(4)(1:1)=''
24660       hfItem$(4)=' '&trim$(hfItem$(4))&' '
24666       ! /r
24680       posMask=pos(lwrc$(hfItem$(4)),' mask=')
24682       posComboF=pos(lwrc$(hfItem$(4)),' combof(') 
24684       posComboA=pos(lwrc$(hfItem$(4)),' comboa(') 
24700       if posMask>0 then 
24710         ! r: masked text box
24720         posSpaceAfter=pos(hfItem$(4),' ',posMask+1)
24722         ! pr hfItem$(4) : pause
24740         mask$=lwrc$(hfItem$(4)(posMask+6:posSpaceAfter-1))
25000         if mask$='currency' or mask$='pointtwo' then
25020           tmp=32
25040         else if mask$='glnumber' then
25060           tmp=53
25080         else if mask$='mmddyy' then
25100           tmp=1
25120         else if mask$='ccyymmdd' then
25140           tmp=3
25160         else if mask$='number' then
25180           tmp=30
25200         else
25220           tmp=val(hfItem$(4)(posMask+6:posSpaceAfter))
25240         end if
25260         if tmp=0 then
25280           if hfFieldType$(hfItem)='N' or hfFieldType$(hfItem)='PD' or hfFieldType$(hfItem)='G' and fp(hfStorageLen(hfItem))=2 then
25300             tmp=32
25320           end if
25340         end if
25360         hfMask(hfitem)+=tmp
25370         ! /r
26000       else if posComboF>0 or posComboA>0 then
26010        debugCombo=0 ! debugCombo=1
26020         ! r: comoboboxes
26040         mat comboBox$(hfItem,udim(mat comboBox$,2))
26060         posComboA=pos(lwrc$(line$),'comboa(')
26080         posComboF=pos(lwrc$(line$),'combof(')
26100         if posComboF>0 then 
26120           posComboX=posComboF 
26300           comboBox$(hfItem,1)='ComboF'                 ! 'ComboF'
28000           pr "comboBox$(hfItem,1)='ComboF'"
28020         else if posComboA then
28040           posComboX=posComboA
28060           comboBox$(hfItem,1)='ComboA'                 ! 'ComboA'
28080           pr "comboBox$(";hfItem;",1)='ComboA'"
28100         else 
28120           pr 'unexpected comboBox err in hamsterFio line$="'&line$&'"' : pause
28140         end if
28160         dim cfItem$(0)*128
28180         mat cfItem$(0)
28200         str2mat(line$(posComboX+len('comboX('):pos(line$,')',posComboX)-1),mat cfItem$,',')
28210         if debugCombo then pr line$ : pr mat cfItem$ : pause
28220         ! r: comboBox$(x,y) Legend for ComoboF
28240           ! X= item number
28260           ! Y=
28280           !    data_file$ 
28300           !    3  key_pos
28320           !    4  key_len ! Key Length
28340           !    5  desc_pos 
28360           !    6  desc_len ! Description Length
28380           !    7  index_file$ 
28400           !    8  limit_to_list
28420         ! /r
28440         for cbIndex=2 to udim(mat cfItem$)
28460           cfItem$(cbIndex)=trim$(cfItem$(cbIndex),"'")
28480           cfItem$(cbIndex)=trim$(cfItem$(cbIndex),'"')
28500           ! cfItem$(cbIndex)=srep$(cfItem$(cbIndex),"env$('Q')",env$('Q'))
28520           ! cfItem$(cbIndex)=srep$(cfItem$(cbIndex),"env$('cno')",env$('cno'))
28540           ! cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'[Q]',env$('Q'))
28560           ! cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'[cno]',env$('cno'))
28570           cfItem$(cbIndex)=fn_srepEnv$(cfItem$(cbIndex))
28580           if pos(cfItem$(cbIndex),'*custom:UB ServiceCodes*')>0 then
28600             library 'S:\Core\Library': fnget_services
28620             dim serviceName$(10)*20
28640             dim serviceCode$(10)*2
28660             fnget_services(mat serviceName$,mat serviceCode$)
28680             dim serviceCsv$*256
28700             serviceCsv$=''
28720             for scItem=1 to udim(mat serviceCode$)
28740               if trim$(serviceCode$(scItem))<>'' then
28760                 if scItem>1 then serviceCsv$(inf:inf)=','
28780                 serviceCsv$(inf:inf)=serviceCode$(scItem)
28800               end if
28820             nex scItem
28860             cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'*custom:UB ServiceCodes*',serviceCsv$)
29000           else if pos(cfItem$(cbIndex),'*custom:UB ServiceCodes Metered*')>0 then
29020             library 'S:\Core\Library': fnGetServiceCodesMetered
29040             fnGetServiceCodesMetered(mat serviceCodeMetered$)
29060             mat2str(mat serviceCodeMetered$,serviceCsv$,',')
29080             cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'*custom:UB ServiceCodes Metered*',serviceCsv$)
29100           end if
29120           comboBox$(hfItem,cbIndex)=cfItem$(cbIndex) 
29122           if debugCombo then pr 'comboBox$(';hfItem;',';cbIndex;') to "'&cfItem$(cbIndex)&'"'
29140         nex cbIndex
29160         if debugCombo then  pause
29180       end if 
29200       ! /r
29220     end if
29240   loop
29260   hfEofhLay: !
29280   close #hLay:
29300 fnend
32000 def fn_srepEnv$*256(text$*256)
32010   do
32020     sePosOpen=pos(text$,'[')
32030     sePosClose=pos(text$,']',sePosOpen)
32040     if sePosOpen>0 and sePosClose>sePosOpen then
32050       seVariable$=text$(sePosOpen+1:sePosClose-1)
32060       text$=srep$(text$,'['&seVariable$&']',env$(seVariable$))
32070     end if
32080   loop while sePosOpen>0 and sePosClose>sePosOpen
32090   fn_srepEnv$=text$
32990 fnend
76000 ! <updateable region: fn_open (supressprompt:=2)>  
76020 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
76040   dim _fileiosubs$(1)*800, loadedsubs$(1)*32
76060   fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
76080   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then 
76100     mat loadedsubs$(udim(loadedsubs$)+1) 
76120     loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
76140     for index=1 to udim(mat _fileiosubs$) 
76160       execute (_fileiosubs$(index)) 
76180     next index
76200   end if
76220 fnend
76240 ! </updateable region: fnopen>
78000 ! <updateable region: ertn>
78020 ERTN: fnerror(program$,err,line,act$,"xit")
78040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
78060   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
78080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
78100 ERTN_EXEC_ACT: execute act$ : goto ERTN
78120 ! </updateable region: ertn>