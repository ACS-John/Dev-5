00010 ! modifies customer records depending upon how the program is configured, sequences services, etc
00030 ! r: setup library, dims, on err, fntop, etc
00040   library 'S:\Core\Library': fntop,fnxit, fnAcs,fnLbl,fnTxt,fnwait,fnTos,fncno,fnxit,fnerror,fnCmdSet,fntop,fnopenprn,fncloseprn,fnub_index_customer,fngethandle
00050   on errror goto ERTN
00110   fntop(program$)
00120  ! /r
00130   ! gosub OldWorkFromFixedWidthList
12000   ! r: primary loop setup
12020   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7,gb(10),extra$(11)*30
12040   dim extra(23)
12042   dim df$*1
12060   open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno]",internal,outIn,relative 
12082   F_CustomerSequenceOnly: form pos 1743,N 7
12100   ! /r
14000   do ! r: primary loop
14020     read_count+=1
14040     read #h_customer,using F_CustomerSequenceOnly: sequenceNumber eof PrimaryFinis
14060     didChange=0
14080     didChange+=fn_multiplySequenceByTen(sequenceNumber)
14120     if didChange then
14140       rewrite #h_customer,using F_CustomerSequenceOnly: sequenceNumber
14160       write_count+=1
14180     end if
14200   loop ! /r
16000   PrimaryFinis: !
16020     pr 'read_count=';read_count
16040     pr 'write_count=';write_count : pause
18000   xit: fnxit
28000 ! <Updateable Region: ERTN>
28060 ERTN: fnerror(program$,err,line,act$,"xit")
28080   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
28100   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
28120   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
28140 ERTN_EXEC_ACT: execute act$ : goto ERTN
28160 ! /region
34000 def fn_multiplySequenceByTen(&sequenceNumber)
34020   drbtReturn=0
34040   if len(str$(sequenceNumber))>999999 then
34060     pr bell;'sequence number ('&str$(sequenceNumber)&') wrong length to convert.'
34080     pause
34160   else
34180     sequenceNumber=sequenceNumber*10
34200     drbtReturn=1
34220   end if
34240   fn_multiplySequenceByTen=drbtReturn
34260 fnend
