00011 ! uw$         unique word -
00012 ! mat lbl$    array of field labels
00014 ! mat fln     array of field lengths
00015 ! fin         open file handle
00016 ! mat p$      array of
00020 def library fnHamster(uw$*20,mat lbl$,mat fln,fin,mat p$; mat flTyp$,mat sln,mat mask,mat startPos,mat incontrol$,mat mxl)
00030   ! r: setup
00040   library 'S:\Core\Library': fnerror,fnTos,fnflexinit1,fnCmdKey,fnAcs,fnflexadd1,fnLbl,fnTxt,fncomboa,fncombof,fnpause,fnChk,fngethandle
00050   on error goto ERTN
00060   ! ______________________________________________________________________
00070   dim tmp$*80,sln2(199),fltyp2$(199)*2
00080   dim mask2(199),startPos2(199),option$(199)*256,control$(60,26)*256
00090   dim p2$(100)*256 ! used to hold mat P$ + 1 more response for Add Loop
00100   dim keyorder(199) ! contains a 0 if not a key, else contains it's sequence in the order of fields used to make the key
00110   dim cmask$(199) ! Flexgrid Column Mask
00120   dim flxItem$(199)*2048,flxhdr$(199)*80 ! flexgrid item and header
00130   dim key$*80,blank$(20)*80 ! dynamically built key
00140   dim keyform$*1000,resp$(256)*1024
00142   dim sn$*256 ! screen name - used to uniquely identify screen... well, it used to be.  it does nothing now.
00150   ! r: prepare arrays
00160     mat flxItem$(199) : mat flxhdr$(199) : mat sln2(199) : mat fltyp2$(199)
00170     mat mask2(199) : mat startPos2(199) : mat option$(199) : mat control$(60,26)
00180     row_select=1 : opt_cancel=5 : opt_add=4 : opt_edit=3
00190     opt_delete=7 : right=1
00200     itemCount=udim(p$)
00202     mat hComboF(itemCount)
00210     ! 
00220     if udim(incontrol$,1)<>0 then 
00230       mat control$(udim(incontrol$,1),udim(incontrol$,2))
00240       mat control$=incontrol$
00250     end if 
00260     if udim(startPos)<>itemCount then 
00270       startPos2(1)=1
00280     else 
00290       startPos2(1)=startPos(1)
00300     end if 
00310     for j=1 to itemCount
00312       if udim(mat control$,1)=>j and lwrc$(control$(j,1))='combof' and control$(j,7)<>'' then ! it is a combof that has an index
00314         fltyp2$(j)="c"
00318         open #hComboF(j):=fngethandle: 'name='&control$(j,2)&',kfname='&control$(j,7)&',Shr',internal,input,keyed 
00320       else if udim(flTyp$)<>itemCount then 
00330         fltyp2$(j)="g"
00340       else if j>udim(flTyp$) then 
00350         fltyp2$(j)="g"
00360       else if flTyp$(j)="" then 
00370         fltyp2$(j)="g"
00380       else 
00390         fltyp2$(j)=lwrc$(flTyp$(j))
00400       end if 
00410       if udim(sln)<>itemCount then 
00420         sln2(j)=fln(j)
00430       else if sln(j)=0 then 
00440         sln2(j)=fln(j)
00450       else 
00460         sln2(j)=sln(j)
00470       end if 
00480       if udim(mask)<>itemCount then 
00490         mask2(j)=0
00500       else 
00510         mask2(j)=mask(j)
00520       end if 
00530       if mask2(j)=1 then fln(j)=8
00540       if j=1 then goto SKIP_startPos
00550       if udim(mat startPos)=itemCount then 
00560         startPos2(j)=startPos(j)
00570       else if udim(mat startPos)<>itemCount and udim(mat startPos)<>0 then 
00580         startPos2(j)=startPos2(j-1)+int(sln2(j-1))
00590       else if udim(mat startPos)<>itemCount and udim(mat startPos)<>0 then 
00600         startPos2(j)=startPos2(j-1)+int(sln2(j-1))
00610       else if udim(mat startPos)=itemCount then 
00620         startPos2(j)=startPos2(j-1)+int(sln2(j-1))
00630       else if udim(startPos)=0 then 
00640         startPos2(j)=startPos2(j-1)+int(sln2(j-1))
00650       else if startPos(j)=0 then 
00660         startPos2(j)=startPos2(j-1)+int(sln2(j-1))
00670       end if 
00680       if udim(mat startPos)=>j and startPos(j)<>0 then 
00690         startPos2(j)=startPos(j)
00700       end if 
00710       SKIP_startPos: ! 
00720     next j
00730     mat mask2(itemCount) : mat sln2(itemCount) : mat fltyp2$(itemCount) : mat startPos2(itemCount) : mat keyorder(itemCount)
00732   ! /r
00740   ! Gosub KEYORDER_BUILD
00750   ! r: Build Flex Headers and Flex Mask
00760     mat flxhdr$(itemCount+1) : fhc=0 : flxhdr$(fhc+=1)='Rec'
00770     for j=2 to itemCount+1
00780       if mask2(j-1)<20000 then flxhdr$(fhc+=1)=lbl$(j-1)
00790       controlX=j-1
00800       testmask=mask2(controlX)
00810       if testmask=>1000 and testmask<2000 then testmask-=1000
00812       if controlX<=udim(mat control$,1) and lwrc$(control$(controlX,1))='combof' and control$(controlX,7)<>'' then 
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
00970       ! 
00980     next j
00990     mat flxhdr$(fhc) : mat flxItem$(fhc) : mat cmask$(fhc)
01000     ! /r
01020   ! /r
01030   ! /r
01010   goto MENU1


01030   ! KEYORDER_BUILD: ! r:
01040   ! uses: FIN, mat startPos2
01050   ! returns: mat keyorder
01060   ! this section is not used currently
01070   ! if later we want to add an option to force keys to be unique,
01080   ! than I'll probably want to resurect and test this section
01090       j=0 : mat keyorder=(0) : bowman=0
01100       do while kps(fin,j+=1)>0
01110         for j=1 to udim(startPos2)
01120           if startPos2=kps(fin,j) then keyorder(j)=bowman+=1
01130         next j
01140       loop 
01150       return 
01160   ! /r
01170   MENU1: ! r:
01180       fnTos(sn$=uw$&"1b")
01190       fnflexinit1(uw$&"2b",1,1,20,108,mat flxhdr$,mat cmask$,row_select)
01200       for j1=1 to lrec(fin)
01210         prec=j1
01212         gosub READ_P ! Read #FIN,Using FRM$,Rec=J1: MAT P$ noRec (just past fnflexadd1)
01220         if pnorec<>1 then 
01230           fic=0 : flxItem$(fic+=1)=str$(rec(fin))
01240           for j2=2 to itemCount+1
01242             controlX=j2-1
01244             if mask2(controlX)<20000 then 
01246               dim hcfDesc$*128,hcfKey$*128
01248               hcfDesc$='' ! p$(controlX)
01250               if controlX<=udim(mat control$,1) and lwrc$(control$(controlX,1))='combof' and control$(controlX,7)<>'' then 
01251                 hcfKey$=rpad$(trim$(p$(controlX))(1:kln(hComboF(controlX))),kln(hComboF(controlX)))
01252                 read #hComboF(controlX),using 'form pos '&control$(controlX,5)&',c '&control$(controlX,6),key=hcfKey$: hcfDesc$ nokey ignore
01253                 hcfDesc$=rtrm$(hcfDesc$)
01254               end if 
01255               flxItem$(fic+=1)=p$(controlX)&' '&hcfDesc$
01256   !           if hcfDesc$<>'' then pr 'flxItem$('&str$(fic)&')="'&flxItem$(fic)&'" hcfDesc$="'&hcfDesc$&'"' : pause
01258             end if 
01260           next j2
01270           fnflexadd1(mat flxItem$)
01272         end if 
01273       next j1
01274       for hComboFitem=1 to hComboFcount
01275         if hComboF(hComboFitem) then 
01276           close #hComboF(hComboFitem): ioerr ignore
01277           hComboF(hComboFitem)=0
01278         end if 
01279       next hComboFitem
01280       fnLbl(21,20," ") ! move command buttons down one line so search box ok
01285       fnCmdKey("Edi&t",opt_edit,1)
01290       fnCmdKey("&Add",opt_add)
01295       fnCmdKey("&Delete",opt_delete)
01300       fnCmdKey("E&xit",opt_cancel,0,1)
01305       fnAcs(sn$,0,mat resp$,menu1_opt)
01310       prec=val(resp$(1)) conv MENU1
01315       if prec=0 and menu1_opt=opt_edit then let menu1_opt=opt_add
01320       if menu1_opt=opt_cancel then 
01325         goto XIT
01330       else if menu1_opt=opt_add then 
01335         goto TO_ADD
01340       else if menu1_opt=opt_edit then 
01345         goto TO_EDIT
01350       else if menu1_opt=opt_delete then 
01355         goto SUB_DELETE
01360       else 
01365         goto MENU1
01370       end if 
01375   ! /r
01380   SUB_DELETE: ! r:
01385       delete #fin,rec=prec: 
01390       goto MENU1
01395   ! /r
01400   TO_EDIT: ! r: ADD and EDIT routines
01405       gosub READ_P
01410   TO_ADD: ! 
01415       if menu1_opt=opt_add then mat p$=("")
01420       if itemCount>30 then 
01425         j2=int(itemCount/2) : myflen=0
01430         for j=1 to j2
01435           myflen=max(myflen,fln(j))
01440         next j
01445       end if 
01450   ! \Print MYFLEN : fnPAUSE ! XXX
01455       mylen=0
01460       for j=1 to itemCount
01465         mylen=max(mylen,len(lbl$(j)))
01470       next j
01475       mat p2$(alana=udim(p$)+1) : mat p2$(1:udim(p$))=p$(1:udim(p$))
01480       fnTos(sn$=uw$&"3")
01485       mypos=mylen+3 : lc=ic=0 : col=1 : colpos=1
01490       for j=1 to itemCount
01495         if itemCount>30 and j>(itemCount/2) and col=1 then 
01500           lc=0 : colpos=mypos+myflen+4 : col+=1
01505           mypos=colpos+mylen+2
01510         end if 
01515         if mask2(ic+1)=>20000 then ic+=1 : goto SKIP_LABEL_AND_CONTROL
01520         fnLbl(lc+=1,colpos,lbl$(ic+=1)&":",mylen,right)
01525         if mask2(ic)>10000 then 
01530           disable=1
01535           mask2(ic)-=10000
01540         else 
01545           disable=0
01550         end if 
01555         if j<udim(mxl) then maxlen=mxl(j) else maxlen=0
01560         if j>udim(control$,1) or trim$(control$(j,1))="" or lwrc$(control$(j,1))="txt" then 
01565           fnTxt(lc,mypos,fln(j),maxlen,0,str$(mask2(ic)),disable) ! p$(j)
01570           goto BROWN
01575         end if 
01580         if lwrc$(control$(j,1))<>"comboa" then goto NOT_A_COMBOA
01585         mat option$(999)
01587         L1160: ! 
01589         cj+=1
01591         if cj<udim(control$,2)-1 and trim$(control$(j,cj))<>"" then 
01593           option$(cj)=control$(j,cj+1)
01595           goto L1160
01597         else 
01599           mat option$(cj-1)
01601         end if 
01603         fncomboa(uw$&"A"&str$(j),lc,mypos,mat option$) ! p$(j)
01605         NOT_A_COMBOA: ! 
01607         if lwrc$(control$(j,1))="combof" then 
01609           fncombof(uw$&"F"&str$(j),lc,mypos,val(control$(j,4))+val(control$(j,6))+3,control$(j,2),val(control$(j,3)),val(control$(j,4)),val(control$(j,5)),val(control$(j,6)),control$(j,7),val(control$(j,8)))
01611         end if 
01613         BROWN: ! done adding control and label
01615         if disable=1 then mask2(ic)+=10000
01617         SKIP_LABEL_AND_CONTROL: ! 
01619       next j
01621       fnLbl(lc+1,20," ") ! move command buttons down one line so search box ok
01623       if menu1_opt=opt_add then 
01625         fnChk(lc+=1,mypos,'Add Loop',right)
01627         p2$(alana)='False'
01629       end if 
01631       if addloop$='' then p2$(alana)='False' else p2$(alana)=addloop$
01633       fnCmdKey("&Save",1,1)
01635       fnCmdKey("&Cancel",opt_cancel,0,1)
01637       ! 
01639       fnAcs(sn$,0,mat p2$,ck)
01641       mat p$(1:udim(p$))=p2$(1:udim(p$))
01643       if ck<>opt_cancel then gosub REWR_P
01645       addloop$=p2$(alana)
01647       if lwrc$(addloop$)=lwrc$('True') then goto TO_ADD else goto MENU1
01649   ! /r
01651   ! <Updateable Region: ERTN>
01653   ERTN: fnerror(program$,err,line,act$,"xit")
01655       if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01657       execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01659       pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01661   ERTN_EXEC_ACT: execute act$ : goto ERTN
01663   ! /region
01665   ! _________________________________________
01667   READ_P: ! r:
01669 ! Pnorec (returned value)= 0 = ok    = 1 = noRec error encountered
01671 ! Peof (returned value)  = 0 = ok    = 1 = EOF   error encountered
01673 ! PRec (sent value)= record number to read
01675     pnorec=0 : peof=0
01677 ! Read 1st Item
01679     j=1
01681     if fltyp2$(j)="c" or fltyp2$(j)="cr" then 
01683       tmp$="Form Pos "&str$(startPos2(j))&",c "&str$(sln2(j))
01685       read #fin,using tmp$,rec=prec,reserve: p$(j) noRec PNOREC eof PEOF
01687     else if fltyp2$(j)="g" then 
01691       tmp$="Form Pos "&str$(startPos2(j))&",g "&str$(sln2(j))
01693       read #fin,using tmp$,rec=prec,reserve: p$(j) noRec PNOREC eof PEOF
01695     else if fltyp2$(j)="n" or fltyp2$(j)="pd" then 
01699       tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "&str$(sln2(j))
01701       read #fin,using tmp$,rec=prec,reserve: t noRec PNOREC eof PEOF
01703       p$(j)=str$(t)
01704     else if fltyp2$(j)="pd" and ord(p$(j))=15 then 
01705       p$(j)=""
01708     end if 
01709 ! Read 2nd to Last Item
01711     for j=2 to itemCount-1
01713       if fltyp2$(j)="c" or fltyp2$(j)="cr" then 
01715         tmp$="Form Pos "&str$(startPos2(j))&",c "&str$(sln2(j))
01717         reread #fin,using tmp$,reserve: p$(j) noRec PNOREC eof PEOF
01719       else if fltyp2$(j)="g" then 
01723         tmp$="Form Pos "&str$(startPos2(j))&",g "&str$(sln2(j))
01725         reread #fin,using tmp$,reserve: p$(j) noRec PNOREC eof PEOF
01727       else if fltyp2$(j)="n" or fltyp2$(j)="pd" then 
01731         tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "&str$(sln2(j))
01733         reread #fin,using tmp$,reserve: t noRec PNOREC eof PEOF
01735         p$(j)=str$(t)
01737       else if fltyp2$(j)="pd" and ord(p$(j))=15 then 
01739         p$(j)=""
01740       end if 
01741     next j
01743 ! read Last Item
01745     j=itemCount
01747     if fltyp2$(j)="c" or fltyp2$(j)="cr" then 
01749       tmp$="Form Pos "&str$(startPos2(j))&",c "&str$(sln2(j))
01751       reread #fin,using tmp$,release: p$(j) noRec PNOREC eof PEOF
01753     else if fltyp2$(j)="g" then 
01757       tmp$="Form Pos "&str$(startPos2(j))&",g "&str$(sln2(j))
01759       reread #fin,using tmp$,release: p$(j) noRec PNOREC eof PEOF
01761     else if fltyp2$(j)="n" or fltyp2$(j)="pd" then 
01765       tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "&str$(sln2(j))
01767       reread #fin,using tmp$,release: t noRec PNOREC eof PEOF
01769       p$(j)=str$(t)
01773     else if fltyp2$(j)="pd" and ord(p$(j))=15 then 
01774       p$(j)=""
01775     end if 
01776     goto READ_P_XIT
01777 PNOREC: pnorec=1 : goto READ_P_XIT
01779 PEOF: peof=1 : goto READ_P_XIT
01781 READ_P_XIT: return 
01783 ! /r
01785   SET_KEY_FORM: ! r:
01787 ! uses: Fin
01789 ! returns: mat blank, keyForm$, key$
01791     keyform$='Form ' : key$='' : j=0
01793     do while kps(fin,j+=1)>0
01795       keyform$=keyform$&'Pos '&str$(kps(fin,j))&','
01797       keyform$=keyform$&'C '&str$(kln(fin,j))&','
01799       blank$(j)=rpt$(chr$(48),kln(fin,j))
01801       key$=key$&blank$(j)
01803     loop 
01805     keyform$=keyform$(1:len(keyform$)-1) ! remove the trailing comma
01807     mat blank$(j-1)
01809 ! pr 'KeyForm$='&KEYFORM$ ! XXX
01811     return 
01813 ! /r
01815   RIGHT_KEY_WRONG_RECORD: ! r:
01817     do  !  L1780: !
01818       read #fin: 
01819     loop until rec(fin)=prec !  if rec(fin)<>prec then goto L1780
01821   return ! /r
01825   SPECIAL_NOKEY: ! r:
01827 ! pr 'Special Nokey routine' ! XXX
01829     key$=""
01831     read #fin,using keyform$,rec=prec: mat blank$
01833     for j=1 to udim(blank$) : key$=key$&blank$(j) : next j
01835     continue  ! not Return  ! not Retry
01837 ! /r
01839   REWR_P: ! r:
01841 ! spos=1
01843     if menu1_opt=opt_add then 
01845       prec=lrec(fin)+1
01847       gosub SET_KEY_FORM
01849       write #fin,using keyform$,reserve: mat blank$
01851 ! .! pr 'write using KeyFormS,Reserve: Mat Blank$   - keyform$='&KEYFORM$
01853       read #fin,key=key$: nokey SPECIAL_NOKEY
01855     else 
01857       gosub SET_KEY_FORM
01859       reread #fin,using keyform$: mat blank$
01861       j=0 : key$=''
01863       do while kps(fin,j+=1)>0
01865         key$=key$&blank$(j)
01867       loop 
01869       read #fin,key=key$: nokey SPECIAL_NOKEY
01871       if rec(fin)<>prec then 
01873         gosub RIGHT_KEY_WRONG_RECORD
01875       end if 
01877     end if 
01879     for j=1 to itemCount
01881       if j<=udim(control$,1) and lwrc$(control$(j,1))="combof" then 
01883         p$(j)=p$(j)(1:val(control$(j,4)))
01885       end if 
01887       crflag=0
01889       if fltyp2$(j)="cr" then 
01891         p$(j)=lpad$(trim$(p$(j)),sln2(j))
01893         fltyp2$(j)="c"
01895         crflag=1
01897       end if 
01899       if lwrc$(fltyp2$(j))<>"pd" then p$(j)=p$(j)(1:sln2(j))
01901       if fltyp2$(j)="c" or fltyp2$(j)="g" or fltyp2$(j)="cr" then 
01903         tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "
01905         tmp$=tmp$&str$(sln2(j))
01907         rewrite #fin,using tmp$,same,reserve: p$(j)
01909 ! .! pr 'Rewr$ - '&TMP$&"   P$("&STR$(J)&")="&P$(J)
01911       end if 
01913       if crflag=1 then fltyp2$(j)="cr" : crflag=0
01915       if fltyp2$(j)="n" or fltyp2$(j)="pd" then 
01917         tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "
01919         tmp$=tmp$&str$(sln2(j)) : t=val(p$(j))
01921         rewrite #fin,using tmp$,same,reserve: t
01923       end if 
01925     next j
01927     release #fin: 
01929 ! REWR_P_XIT: !
01930     return 
01931 ! /r
01933   XIT: ! 
01935 fnend 
01937 ! _________________________________________
