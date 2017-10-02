00011 ! uw$         unique word -
00012 ! mat lbl$    array of field labels
00014 ! mat fln     array of field lengths
00015 ! fin         open file handle
00016 ! mat p$      array of
00020   def library fnhamster(uw$*20,mat lbl$,mat fln,fin,mat p$; mat fltyp$,mat sln,mat mask,mat startpos,mat incontrol$,mat mxl)
00030 ! ______________________________________________________________________
00040     library 'S:\Core\Library': fnerror,fntos,fnflexinit1,fncmdkey,fnacs,fnflexadd1,fnlbl,fntxt,fncomboa,fncombof,fnpause,fnchk,fngethandle
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim tmp$*80,sln2(199),fltyp2$(199)*2
00080     dim mask2(199),startpos2(199),option$(199)*40,control$(60,26)*256
00090     dim p2$(100)*256 ! used to hold mat P$ + 1 more response for Add Loop
00100     dim keyorder(199) ! contains a 0 if not a key, else contains it's sequence in the order of fields used to make the key
00110     dim cmask$(199) ! Flexgrid Column Mask
00120     dim flxitm$(199)*512,flxhdr$(199)*80 ! flexgrid item and header
00130     dim key$*80,blank$(20)*80 ! dynamically built key
00140     dim keyform$*1000,resp$(256)*1024
00142     dim sn$*256 ! screen name - used to uniquely identify screen... well, it used to be.  it does nothing now.
00150 ! r: prepare arrays
00160     mat flxitm$(199) : mat flxhdr$(199) : mat sln2(199) : mat fltyp2$(199)
00170     mat mask2(199) : mat startpos2(199) : mat option$(199) : mat control$(60,26)
00180     let row_select=1 : let opt_cancel=5 : let opt_add=4 : let opt_edit=3
00190     let opt_delete=7 : let right=1
00200     let itemcount=udim(p$)
00202     mat hcombof(itemcount)
00210 ! 
00220     if udim(incontrol$,1)<>0 then 
00230       mat control$(udim(incontrol$,1),udim(incontrol$,2))
00240       mat control$=incontrol$
00250     end if 
00260     if udim(startpos)<>itemcount then 
00270       let startpos2(1)=1
00280     else 
00290       let startpos2(1)=startpos(1)
00300     end if 
00310     for j=1 to itemcount
00312       if lwrc$(control$(j,1))='combof' and control$(j,7)<>'' then ! it is a combof that has an index
00314         let fltyp2$(j)="c"
00318         open #hcombof(j):=fngethandle: 'name='&control$(j,2)&',kfname='&control$(j,7)&',Shr',internal,input,keyed 
00320       else if udim(fltyp$)<>itemcount then 
00330         let fltyp2$(j)="g"
00340       else if j>udim(fltyp$) then 
00350         let fltyp2$(j)="g"
00360       else if fltyp$(j)="" then 
00370         let fltyp2$(j)="g"
00380       else 
00390         let fltyp2$(j)=lwrc$(fltyp$(j))
00400       end if 
00410       if udim(sln)<>itemcount then 
00420         let sln2(j)=fln(j)
00430       else if sln(j)=0 then 
00440         let sln2(j)=fln(j)
00450       else 
00460         let sln2(j)=sln(j)
00470       end if 
00480       if udim(mask)<>itemcount then 
00490         let mask2(j)=0
00500       else 
00510         let mask2(j)=mask(j)
00520       end if 
00530       if mask2(j)=1 then let fln(j)=8
00540       if j=1 then goto SKIP_STARTPOS
00550       if udim(mat startpos)=itemcount then 
00560         let startpos2(j)=startpos(j)
00570       else if udim(mat startpos)<>itemcount and udim(mat startpos)<>0 then 
00580         let startpos2(j)=startpos2(j-1)+int(sln2(j-1))
00590       else if udim(mat startpos)<>itemcount and udim(mat startpos)<>0 then 
00600         let startpos2(j)=startpos2(j-1)+int(sln2(j-1))
00610       else if udim(mat startpos)=itemcount then 
00620         let startpos2(j)=startpos2(j-1)+int(sln2(j-1))
00630       else if udim(startpos)=0 then 
00640         let startpos2(j)=startpos2(j-1)+int(sln2(j-1))
00650       else if startpos(j)=0 then 
00660         let startpos2(j)=startpos2(j-1)+int(sln2(j-1))
00670       end if 
00680       if udim(mat startpos)=>j and startpos(j)<>0 then 
00690         let startpos2(j)=startpos(j)
00700       end if 
00710 SKIP_STARTPOS: ! 
00720     next j
00730     mat mask2(itemcount) : mat sln2(itemcount) : mat fltyp2$(itemcount) : mat startpos2(itemcount) : mat keyorder(itemcount)
00732 ! /r
00740 ! Gosub KEYORDER_BUILD
00750 ! r: Build Flex Headers and Flex Mask
00760     mat flxhdr$(itemcount+1) : let fhc=0 : let flxhdr$(fhc+=1)='Rec'
00770     for j=2 to itemcount+1
00780       if mask2(j-1)<20000 then let flxhdr$(fhc+=1)=lbl$(j-1)
00790       controlx=j-1
00800       let testmask=mask2(controlx)
00810       if testmask=>1000 and testmask<2000 then let testmask-=1000
00812       if lwrc$(control$(controlx,1))='combof' and control$(controlx,7)<>'' then 
00820       else if testmask=>1 and testmask<=29 then 
00830         cmask$(fhc)=str$(testmask)
00840       else if testmask=>30 and testmask<=31 then 
00850         cmask$(fhc)=str$(testmask)
00860       else if testmask=32 then 
00870         cmask$(fhc)=str$(10)
00880       else if testmask=>33 and testmask<=39 then 
00890         cmask$(fhc)=str$(testmask)
00900       else if testmask=>40 and testmask<=49 then 
00910         cmask$(fhc)=str$(testmask)
00920       else if testmask=>50 and testmask<=53 then 
00930         cmask$(fhc)=str$(testmask)
00940       else 
00950         cmask$(fhc)='80'
00960       end if 
00970 ! 
00980     next j
00990     mat flxhdr$(fhc) : mat flxitm$(fhc) : mat cmask$(fhc)
01000 ! /r
01010     goto MENU1
01020 ! ______________________________________________________________________
01030 ! KEYORDER_BUILD: ! r:
01040 ! uses: FIN, mat startpos2
01050 ! returns: mat keyorder
01060 ! this section is not used currently
01070 ! if later we want to add an option to force keys to be unique,
01080 ! than I'll probably want to resurect and test this section
01090     let j=0 : mat keyorder=(0) : bowman=0
01100     do while kps(fin,j+=1)>0
01110       for j=1 to udim(startpos2)
01120         if startpos2=kps(fin,j) then let keyorder(j)=bowman+=1
01130       next j
01140     loop 
01150     return 
01160 ! /r
01170 MENU1: ! r:
01180     let fntos(sn$=uw$&"1b")
01190     let fnflexinit1(uw$&"2b",1,1,20,108,mat flxhdr$,mat cmask$,row_select)
01200     for j1=1 to lrec(fin)
01210       let prec=j1
01212       gosub READ_P ! Read #FIN,Using FRM$,Rec=J1: MAT P$ Norec (just past fnflexadd1)
01220       if pnorec<>1 then 
01230         let fic=0 : let flxitm$(fic+=1)=str$(rec(fin))
01240         for j2=2 to itemcount+1
01242           controlx=j2-1
01244           if mask2(controlx)<20000 then 
01246             dim hcfdesc$*128,hcfkey$*128
01248             let hcfdesc$='' ! p$(controlx)
01250             if lwrc$(control$(controlx,1))='combof' and control$(controlx,7)<>'' then 
01251               let hcfkey$=rpad$(trim$(p$(controlx))(1:kln(hcombof(controlx))),kln(hcombof(controlx)))
01252               read #hcombof(controlx),using 'form pos '&control$(controlx,5)&',c '&control$(controlx,6),key=hcfkey$: hcfdesc$ nokey ignore
01253               let hcfdesc$=rtrm$(hcfdesc$)
01254             end if 
01255             let flxitm$(fic+=1)=p$(controlx)&' '&hcfdesc$
01256 !           if hcfDesc$<>'' then pr 'flxitm$('&str$(fic)&')="'&flxitm$(fic)&'" hcfDesc$="'&hcfDesc$&'"' : pause
01258           end if 
01260         next j2
01270         let fnflexadd1(mat flxitm$)
01272       end if 
01273     next j1
01274     for hcombofitem=1 to hcombofcount
01275       if hcombof(hcombofitem) then 
01276         close #hcombof(hcombofitem): ioerr ignore
01277         let hcombof(hcombofitem)=0
01278       end if 
01279     next hcombofitem
01280     let fnlbl(21,20," ") ! move command buttons down one line so search box ok
01285     let fncmdkey("Edi&t",opt_edit,1)
01290     let fncmdkey("&Add",opt_add)
01295     let fncmdkey("&Delete",opt_delete)
01300     let fncmdkey("E&xit",opt_cancel,0,1)
01305     let fnacs(sn$,0,mat resp$,menu1_opt)
01310     let prec=val(resp$(1)) conv MENU1
01315     if prec=0 and menu1_opt=opt_edit then let menu1_opt=opt_add
01320     if menu1_opt=opt_cancel then 
01325       goto XIT
01330     else if menu1_opt=opt_add then 
01335       goto TO_ADD
01340     else if menu1_opt=opt_edit then 
01345       goto TO_EDIT
01350     else if menu1_opt=opt_delete then 
01355       goto SUB_DELETE
01360     else 
01365       goto MENU1
01370     end if 
01375 ! /r
01380 SUB_DELETE: ! r:
01385     delete #fin,rec=prec: 
01390     goto MENU1
01395 ! /r
01400 TO_EDIT: ! r: ADD and EDIT routines
01405     gosub READ_P
01410 TO_ADD: ! 
01415     if menu1_opt=opt_add then mat p$=("")
01420     if itemcount>30 then 
01425       let j2=int(itemcount/2) : let myflen=0
01430       for j=1 to j2
01435         let myflen=max(myflen,fln(j))
01440       next j
01445     end if 
01450 ! \Print MYFLEN : Let FNPAUSE ! XXX
01455     let mylen=0
01460     for j=1 to itemcount
01465       let mylen=max(mylen,len(lbl$(j)))
01470     next j
01475     mat p2$(alana=udim(p$)+1) : mat p2$(1:udim(p$))=p$(1:udim(p$))
01480     let fntos(sn$=uw$&"3")
01485     let mypos=mylen+3 : let lc=ic=0 : col=1 : colpos=1
01490     for j=1 to itemcount
01495       if itemcount>30 and j>(itemcount/2) and col=1 then 
01500         let lc=0 : colpos=mypos+myflen+4 : col+=1
01505         let mypos=colpos+mylen+2
01510       end if 
01515       if mask2(ic+1)=>20000 then let ic+=1 : goto SKIP_LABEL_AND_CONTROL
01520       let fnlbl(lc+=1,colpos,lbl$(ic+=1)&":",mylen,right)
01525       if mask2(ic)>10000 then 
01530         let disable=1
01535         let mask2(ic)-=10000
01540       else 
01545         let disable=0
01550       end if 
01555       if j<udim(mxl) then let maxlen=mxl(j) else let maxlen=0
01560       if j>udim(control$,1) or trim$(control$(j,1))="" or lwrc$(control$(j,1))="txt" then 
01565         let fntxt(lc,mypos,fln(j),maxlen,0,str$(mask2(ic)),disable) ! p$(j)
01570         goto BROWN
01575       end if 
01580       if lwrc$(control$(j,1))<>"comboa" then goto NOT_A_COMBOA
01585       mat option$(999)
01587 L1160: ! 
01589       cj+=1
01591       if cj<udim(control$,2)-1 and trim$(control$(j,cj))<>"" then 
01593         let option$(cj)=control$(j,cj+1)
01595         goto L1160
01597       else 
01599         mat option$(cj-1)
01601       end if 
01603       let fncomboa(uw$&"A"&str$(j),lc,mypos,mat option$) ! p$(j)
01605 NOT_A_COMBOA: ! 
01607       if lwrc$(control$(j,1))="combof" then 
01609         let fncombof(uw$&"F"&str$(j),lc,mypos,val(control$(j,4))+val(control$(j,6))+3,control$(j,2),val(control$(j,3)),val(control$(j,4)),val(control$(j,5)),val(control$(j,6)),control$(j,7),val(control$(j,8)))
01611       end if 
01613 BROWN: ! done adding control and label
01615       if disable=1 then let mask2(ic)+=10000
01617 SKIP_LABEL_AND_CONTROL: ! 
01619     next j
01621     let fnlbl(lc+1,20," ") ! move command buttons down one line so search box ok
01623     if menu1_opt=opt_add then 
01625       let fnchk(lc+=1,mypos,'Add Loop',right)
01627       let p2$(alana)='False'
01629     end if 
01631     if addloop$='' then let p2$(alana)='False' else let p2$(alana)=addloop$
01633     let fncmdkey("&Save",1,1)
01635     let fncmdkey("&Cancel",opt_cancel,0,1)
01637 ! 
01639     let fnacs(sn$,0,mat p2$,ck)
01641     mat p$(1:udim(p$))=p2$(1:udim(p$))
01643     if ck<>opt_cancel then gosub REWR_P
01645     addloop$=p2$(alana)
01647     if lwrc$(addloop$)=lwrc$('True') then goto TO_ADD else goto MENU1
01649 ! /r
01651 ! <Updateable Region: ERTN>
01653 ERTN: let fnerror(program$,err,line,act$,"xit")
01655     if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01657     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01659     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01661 ERTN_EXEC_ACT: execute act$ : goto ERTN
01663 ! /region
01665 ! _________________________________________
01667 READ_P: ! r:
01669 ! Pnorec (returned value)= 0 = ok    = 1 = NoRec error encountered
01671 ! Peof (returned value)  = 0 = ok    = 1 = EOF   error encountered
01673 ! PRec (sent value)= record number to read
01675     let pnorec=0 : let peof=0
01677 ! Read 1st Item
01679     let j=1
01681     if fltyp2$(j)="c" or fltyp2$(j)="cr" then 
01683       let tmp$="Form Pos "&str$(startpos2(j))&",c "&str$(sln2(j))
01685       read #fin,using tmp$,rec=prec,reserve: p$(j) norec PNOREC eof PEOF
01687     else if fltyp2$(j)="g" then 
01691       let tmp$="Form Pos "&str$(startpos2(j))&",g "&str$(sln2(j))
01693       read #fin,using tmp$,rec=prec,reserve: p$(j) norec PNOREC eof PEOF
01695     else if fltyp2$(j)="n" or fltyp2$(j)="pd" then 
01699       let tmp$="Form Pos "&str$(startpos2(j))&","&fltyp2$(j)&" "&str$(sln2(j))
01701       read #fin,using tmp$,rec=prec,reserve: t norec PNOREC eof PEOF
01703       let p$(j)=str$(t)
01704     else if fltyp2$(j)="pd" and ord(p$(j))=15 then 
01705       let p$(j)=""
01708     end if 
01709 ! Read 2nd to Last Item
01711     for j=2 to itemcount-1
01713       if fltyp2$(j)="c" or fltyp2$(j)="cr" then 
01715         let tmp$="Form Pos "&str$(startpos2(j))&",c "&str$(sln2(j))
01717         reread #fin,using tmp$,reserve: p$(j) norec PNOREC eof PEOF
01719       else if fltyp2$(j)="g" then 
01723         let tmp$="Form Pos "&str$(startpos2(j))&",g "&str$(sln2(j))
01725         reread #fin,using tmp$,reserve: p$(j) norec PNOREC eof PEOF
01727       else if fltyp2$(j)="n" or fltyp2$(j)="pd" then 
01731         let tmp$="Form Pos "&str$(startpos2(j))&","&fltyp2$(j)&" "&str$(sln2(j))
01733         reread #fin,using tmp$,reserve: t norec PNOREC eof PEOF
01735         let p$(j)=str$(t)
01737       else if fltyp2$(j)="pd" and ord(p$(j))=15 then 
01739         let p$(j)=""
01740       end if 
01741     next j
01743 ! read Last Item
01745     let j=itemcount
01747     if fltyp2$(j)="c" or fltyp2$(j)="cr" then 
01749       let tmp$="Form Pos "&str$(startpos2(j))&",c "&str$(sln2(j))
01751       reread #fin,using tmp$,release: p$(j) norec PNOREC eof PEOF
01753     else if fltyp2$(j)="g" then 
01757       let tmp$="Form Pos "&str$(startpos2(j))&",g "&str$(sln2(j))
01759       reread #fin,using tmp$,release: p$(j) norec PNOREC eof PEOF
01761     else if fltyp2$(j)="n" or fltyp2$(j)="pd" then 
01765       let tmp$="Form Pos "&str$(startpos2(j))&","&fltyp2$(j)&" "&str$(sln2(j))
01767       reread #fin,using tmp$,release: t norec PNOREC eof PEOF
01769       let p$(j)=str$(t)
01773     else if fltyp2$(j)="pd" and ord(p$(j))=15 then 
01774       let p$(j)=""
01775     end if 
01776     goto READ_P_XIT
01777 PNOREC: let pnorec=1 : goto READ_P_XIT
01779 PEOF: let peof=1 : goto READ_P_XIT
01781 READ_P_XIT: return 
01783 ! /r
01785 SET_KEY_FORM: ! r:
01787 ! uses: Fin
01789 ! returns: mat blank, keyForm$, key$
01791     let keyform$='Form ' : let key$='' : let j=0
01793     do while kps(fin,j+=1)>0
01795       let keyform$=keyform$&'Pos '&str$(kps(fin,j))&','
01797       let keyform$=keyform$&'C '&str$(kln(fin,j))&','
01799       blank$(j)=rpt$(chr$(48),kln(fin,j))
01801       let key$=key$&blank$(j)
01803     loop 
01805     let keyform$=keyform$(1:len(keyform$)-1) ! remove the trailing comma
01807     mat blank$(j-1)
01809 ! pr 'KeyForm$='&KEYFORM$ ! XXX
01811     return 
01813 ! /r
01815 RIGHT_KEY_WRONG_RECORD: ! r:
01817     do  !  L1780: !
01818       read #fin: 
01819     loop until rec(fin)=prec !  if rec(fin)<>prec then goto L1780
01821     return 
01823 ! /r
01825 SPECIAL_NOKEY: ! r:
01827 ! pr 'Special Nokey routine' ! XXX
01829     let key$=""
01831     read #fin,using keyform$,rec=prec: mat blank$
01833     for j=1 to udim(blank$) : let key$=key$&blank$(j) : next j
01835     continue  ! not Return  ! not Retry
01837 ! /r
01839 REWR_P: ! r:
01841 ! let spos=1
01843     if menu1_opt=opt_add then 
01845       let prec=lrec(fin)+1
01847       gosub SET_KEY_FORM
01849       write #fin,using keyform$,reserve: mat blank$
01851 ! .! pr 'write using KeyFormS,Reserve: Mat Blank$   - keyform$='&KEYFORM$
01853       read #fin,key=key$: nokey SPECIAL_NOKEY
01855     else 
01857       gosub SET_KEY_FORM
01859       reread #fin,using keyform$: mat blank$
01861       let j=0 : let key$=''
01863       do while kps(fin,j+=1)>0
01865         let key$=key$&blank$(j)
01867       loop 
01869       read #fin,key=key$: nokey SPECIAL_NOKEY
01871       if rec(fin)<>prec then 
01873         gosub RIGHT_KEY_WRONG_RECORD
01875       end if 
01877     end if 
01879     for j=1 to itemcount
01881       if j<=udim(control$,1) and lwrc$(control$(j,1))="combof" then 
01883         let p$(j)=p$(j)(1:val(control$(j,4)))
01885       end if 
01887       crflag=0
01889       if fltyp2$(j)="cr" then 
01891         let p$(j)=lpad$(trim$(p$(j)),sln2(j))
01893         let fltyp2$(j)="c"
01895         crflag=1
01897       end if 
01899       if lwrc$(fltyp2$(j))<>"pd" then let p$(j)=p$(j)(1:sln2(j))
01901       if fltyp2$(j)="c" or fltyp2$(j)="g" or fltyp2$(j)="cr" then 
01903         let tmp$="Form Pos "&str$(startpos2(j))&","&fltyp2$(j)&" "
01905         let tmp$=tmp$&str$(sln2(j))
01907         rewrite #fin,using tmp$,same,reserve: p$(j)
01909 ! .! pr 'Rewr$ - '&TMP$&"   P$("&STR$(J)&")="&P$(J)
01911       end if 
01913       if crflag=1 then let fltyp2$(j)="cr" : crflag=0
01915       if fltyp2$(j)="n" or fltyp2$(j)="pd" then 
01917         let tmp$="Form Pos "&str$(startpos2(j))&","&fltyp2$(j)&" "
01919         let tmp$=tmp$&str$(sln2(j)) : let t=val(p$(j))
01921         rewrite #fin,using tmp$,same,reserve: t
01923       end if 
01925     next j
01927     release #fin: 
01929 ! REWR_P_XIT: !
01930     return 
01931 ! /r
01933 XIT: ! 
01935   fnend 
01937 ! _________________________________________
