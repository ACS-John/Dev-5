12000 def fn_setup
12020   if ~setup then
12040     setup=1
12060     library 'S:\Core\Library': fnAddOneC,fnmsgbox,fnOpenFile,fncreg_read,fncreg_write,fnAddOneN,fntop,fnxit,fnhamster,fngethandle,fnCloseFile,fnerror,fnreg_read,fnreg_write,fnindex_it,fnstatus_close,fnstatus,fnstatus_pause
12080     dim form$(0)*256
12100     dim maData$(0)*128,maDataN(0)
12120     dim mg$(0)*128
12120     dim hfLabel$(0)*128
12480   end if
12900 fnend
22000 def library fnHamsterFio(fileid$*64)
22020   if ~setup then let fn_setup
22040   dim DefaultFileLayoutPath$*256
22060   DefaultFileLayoutPath$="S:\Core\FileIO\Layout\"
22080   DefaultFileLayoutExtension$=".fio"
22100   dim hfData$(0)*256
22120   dim hfDataN(0)
22140   dim hfDataAll$(0)*256
22160   hFile=fn_open(fileid$,mat hfData$,mat hfDataN,mat form$)
22180   if hFile then
22200     fn_hfLayoutRead(DefaultFileLayoutPath$&fileid$&DefaultFileLayoutExtension$,mat hfDataAll$,mat hfLabel$,mat hfFieldType$,mat hfStorageLen,mat hfMask,mat hfFieldLen)
22220     fnhamster(fileid$,mat hfLabel$,mat hfFieldLen,hFile,mat hfDataAll$,mat hfFieldType$,mat hfStorageLen,mat hfMask)
22240   end if
22260 fnend
24000 def fn_hfLayoutRead(hfLayoutFilename$*256,mat hfDataAll$,mat hfLabel$,mat hfFieldType$,mat hfStorageLen,mat hfMask,mat hfFieldLen)
24020   dim tmp$*1024,hfitem$(0)*1024
24040   open #hlay:=fngethandle: 'name='&hfLayoutFilename$,d,i
24060   past_header=0
24080   hfItem=0
24100   mat hfDataAll$(0) : mat hfLabel$(0) : mat hfFieldType$(0) : mat hfStorageLen(0) : mat hfMask(0) : mat hfFieldLen(0) 
24120   do
24140     linput #hlay: tmp$ 
24160     if tmp$(1:6)='======' then past_header=1
24180   loop until past_header
24200   do
24220     linput #hlay: tmp$ eof hfEofhlay
24240     if trim$(tmp$)(1:1)<>'!' and trim$(tmp$)<>'' and trim$(tmp$)(1:1)<>'#' then
24260     str2mat(tmp$,mat hfitem$,',')
24280       hfItem+=1
24300       fnAddOneC(mat hfDataAll$,'')
24320       fnAddOneC(mat hfLabel$,trim$(hfitem$(2)))
24340       hfitem$(3)=trim$(hfitem$(3))
24360       posSpace=pos(hfitem$(3),' ')
24380       fnAddOneC(mat hfFieldType$,uprc$(trim$(hfitem$(3)(1:posSpace-1))))
24400       hfitem$(3)(1:posSpace-1)=''
24420       hfitem$(3)=trim$(hfitem$(3))
24440       fnAddOneN(mat hfStorageLen,val(hfitem$(3)))
24460       fnAddOneN(mat hfMask,0) ! accumulated last
24480       tmp=int(hfStorageLen(hfItem))
24500       if uprc$(hfFieldType$(hfItem))='PD' then tmp=tmp*2-1
24520       fnAddOneN(mat hfFieldLen,tmp)
24540       if pos(lwrc$(hfitem$(4)),' required=true')>0 then hfMask(hfitem)+=1000
24560       do while pos(hfitem$(4),'= ')>0
24580         hfitem$(4)=srep$(hfitem$(4),'= ','=')
24600       loop 
24620       hfitem$(4)=trim$(hfitem$(4))
24640       if hfitem$(4)(1:1)='!' then hfitem$(4)(1:1)=''
24660       hfitem$(4)=' '&trim$(hfitem$(4))&' '
24680       posMask=pos(lwrc$(hfitem$(4)),' mask=')
24700       if posMask>0 then
24720         posSpaceAfter=pos(hfitem$(4),' ',posMask+1)
24722 ! pr hfitem$(4) : pause
24740         mask$=lwrc$(hfitem$(4)(posMask+6:posSpaceAfter-1))
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
25220           tmp=val(hfitem$(4)(posMask+6:posSpaceAfter))
25240         end if
25260         if tmp=0 then
25280           if hfFieldType$(hfItem)='N' or hfFieldType$(hfItem)='PD' or hfFieldType$(hfItem)='G' and fp(hfStorageLen(hfItem))=2 then
25300             tmp=32
25320           end if
25340         end if
25360         hfMask(hfitem)+=tmp
25380       end if
25400     end if
25420   loop
25440   hfEofhlay: !
25460   close #hlay:
25480 fnend
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