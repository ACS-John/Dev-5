00010 ! Replace S:\acsGL\Reverse
00020 !
00030   library 'S:\Core\Library': fntop,fnxit, fnerror,fnLbl,fnTxt,fnAcs,fnCmdKey,fnTos,fnChk
00040   fntop(program$,cap$="Generate Reversing Entries")
00050   on error goto Ertn
00060 !
00070   dim cnam$*40,cap$*128,wait_message$*40
00080   dim t$*12,n(2),l$*12,adr(2),ta(2),p$*30,my_p$*30,test_p$*30,io1$(7)*25
00090 !
00110   wait_message$="Generating Reversing Entries"
00120 ! 
00130   my_p$="Generated Reversing Entry"
00140 !
00150 MENU1: ! 
00160   fnTos(sn$="Reverse") !:
        mylen=20: mypos=mylen+3 : right=1
00170   fnLbl(1,10,"Search For (blank for all)")
00180   fnLbl(2,1,"Adjustment Date:",mylen,right)
00190   fnTxt(2,mypos,8,0,right,"1",0,"If you are wanting to reverse some specific adjustments and can identify them by date, use that date.",0 ) !:
        resp$(1)=str$(s_ad)
00200   fnLbl(3,1,"Reference Number:",mylen,right)
00210   fnTxt(3,mypos,12,0,right,"",0,"Enter the reference # of the adjustment to reverse a specific adjustment.",0 ) !:
        resp$(2)=s_rn$
00220   fnLbl(4,1,"Transaction Code:",mylen,right)
00230   fnTxt(4,mypos,1,0,right,"30",0,"If you wish to reverse all adjustments, you can enter a transaction code of 3.",0 ) !:
        resp$(3)=str$(s_tc)
00240   fnChk(6,mypos,"Search History Also:",right) !:
        resp$(4)=sh$
00250   fnLbl(8,5,"Reverse With (blank for no change)")
00260   fnLbl(9,1,"Adjustment Date:",mylen,right)
00270   fnTxt(9,mypos,8,0,right,"1",0,"",0 ) !:
        resp$(5)=str$(r_ad)
00280   fnLbl(10,1,"Reference Number:",mylen,right)
00290   fnTxt(10,mypos,12,0,right,"",0,"",0 ) !:
        resp$(6)=r_rn$
00300   fnChk(11,mypos,"Reverse Entry Now:",right) !:
        resp$(7)="True"
00310   fnCmdKey("&Next",1,1,0,"Proceed with reversing adjustments.")
00320   fnCmdKey("&Cancel",5,0,1,"Return to menu without reversing.")
00330   fnAcs(sn$,0,mat resp$,ckey)
00340   if ckey=5 then goto XIT
00350   s_ad=val(resp$(1))
00360   s_rn$=resp$(2)
00370   s_tc=val(resp$(3))
00380   if resp$(4)="True" then sh$="Y" else sh$="N"
00390   r_ad=val(resp$(5))
00400   r_rn$=resp$(6)
00410   if resp$(7)="True" then re$="Y" else re$="N"
00420   s_rn$=uprc$(rtrm$(ltrm$(s_rn$)))
00430 !
00440   glmstr=1 !:
        open #glmstr: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed 
00450   gltrans=2 !:
        open #gltrans: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,outIn,relative 
00460   actrans=3 !:
        open #actrans: "Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Shr",internal,outIn,keyed 
00470   x=lrec(2)
00480 ! ____
00490   for j=1 to x
00500 READ_GLTRANS: ! 
00510     read #gltrans,using L520,rec=j: t$,s,k,mat n,l$,p$ eof HIST noRec NEXT_GLTRANS
00520 L520: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
00530     gosub SEARCH_FOR : if s_pass=0 then goto NEXT_GLTRANS
00540     gosub REVERSE_WITH
00550     read #glmstr,using L560,key=t$: cb,mat ta nokey NEXT_GLTRANS
00560 L560: form pos 87,pd 6.2,pos 333,2*pd 3
00570 L570: lr2=lrec(2)+1
00580     write #gltrans,using L520,rec=lr2: t$,s,k,mat n,l$,mp_p$,0 duprec L570
00590     if ta(1)=0 then ta(1)=lr2
00600     if ta(2)>0 then !:
            rewrite #gltrans,using L640,rec=ta(2): lr2
00610     ta(2)=lr2
00620     cb=cb+k
00630     rewrite #glmstr,using L560,key=t$: cb,mat ta
00640 L640: form pos 71,pd 3
00650 NEXT_GLTRANS: next j
00660 !
00670 HIST: if sh$="N" then goto DONE
00680 READ_ACTRANS: read #actrans,using L520: t$,s,k,mat n,l$,p$ eof DONE
00690   gosub SEARCH_FOR : if s_pass=0 then goto NXT_HIST
00700   gosub REVERSE_WITH
00710   read #glmstr,using L560,key=t$: cb,mat ta nokey NXT_HIST
00720 L720: lr2=lrec(2)+1
00730   write #gltrans,using L520,rec=lr2: t$,s,k,mat n,l$,my_p$,0 duprec L720
00740   if ta(1)=0 then ta(1)=lr2
00750   if ta(2)>0 then rewrite #gltrans,using L640,rec=ta(2): lr2
00760   ta(2)=lr2
00770   cb=cb+k
00780   rewrite #glmstr,using L560,key=t$: cb,mat ta
00790 NXT_HIST: goto READ_ACTRANS
00800 !
00810 DONE: ! 
00820   close #win: ioerr L830
00830 L830: goto XIT
00840 !
00850 XIT: fnxit
00860 !
00870 ! <Updateable Region: ERTN>
00880 ERTN: fnerror(program$,err,line,act$,"xit")
00890   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00900   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00910   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00920 ERTN_EXEC_ACT: execute act$ : goto ERTN
00930 ! /region
00940 !
00950 SEARCH_FOR: ! 
00960 ! 
00970   test_p$=uprc$(rtrm$(ltrm$(p$)))
00980   test_l$=uprc$(rtrm$(ltrm$(l$)))
00990 ! 
01000   if test_p$<>uprc$(rtrm$(ltrm$(my_p$))) then s_test_p=1
01010   if test_p$=uprc$(rtrm$(ltrm$(my_p$))) then s_test_p=0
01020   if s_ad=0 then s_test_ad=1
01030   if s_ad<>0 and s_ad=s then s_test_ad=1
01040   if s_ad<>0 and s_ad<>s then s_test_ad=0
01050   if s_tc=0 then s_test_tc=1
01060   if s_tc<>0 and s_tc=n(1) then s_test_tc=1
01070   if s_tc<>0 and s_tc<>n(1) then s_test_tc=0
01080   if s_rn$="" then s_test_rn=1
01090   if s_rn$<>"" and s_rn$=test_l$ then s_test_rn=1
01100   if s_rn$<>"" and s_rn$<>test_l$ then s_test_rn=0
01110   if s_test_ad=1 and s_test_tc=1 and s_test_rn=1 and s_test_p=1 then !:
          s_pass=1 !      Pass                            !:
        else s_pass=0 ! Fail
01120   return 
01130 !
01140 REVERSE_WITH: ! 
01150   if re$="Y" then k=-k
01160   if r_ad<>0 then s=r_ad
01170   if r_rn$<>"" then l$=r_rn$
01180   if t$(3:3)=" " then t$(3:3)="0"
01190   if t$(12:12)=" " then t$(12:12)="0"
01200   return 
01210 !
