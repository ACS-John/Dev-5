! formerly S:\acsUB\Per1000
! -- Per 1000 Usage Report
! ______________________________________________________________________
  library 'S:\Core\Library': fntop,fnxit, fnAcs,fnLbl,fnTxt,fnTos,fnerror,fnopenprn,fncloseprn,fnLastBillingDate,fncomboa,fnCmdSet,fnget_services,fncreg_read,fncreg_write
  on error goto ERTN
! ______________________________________________________________________
  dim z$*10,e$(4)*30,g(12)
  dim a(7),d(15)
  dim range(16),excess(2999,2),cust(16),over(160)
  dim resp$(20)*40,text$*40
  dim serviceName$(10)*20
! ______________________________________________________________________
  fntop(program$)
  fnLastBillingDate(d1)
! 
  fnget_services(mat serviceName$)
  mat opt$(3)
  opt$(1)="Water"
  opt$(2)="Gas"
  if trim$(serviceName$(3))="Lawn Meter" then 
    stfn$="wgl"
    opt$(3)="Lawn Meter"
  else 
    stfn$="wge"
    opt$(3)="Electric"
  end if 
! 
  gosub USAGE_CHART_ASK_RANGE
  open #customer:=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
! on fkey 99 goto DONE
! on fkey 5 goto DONE
  on pageoflow goto PGOF
  fnopenprn
goto READ_CUSTOMER
READ_CUSTOMER: ! r:
  read #customer,using F_CUSTOMER: z$,mat e$,mat a,mat d,bal,f,mat g eof ENDUBM
F_CUSTOMER: form pos 1,c 10,4*c 30,pos 143,7*pd 2,pos 217,15*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2
  if weg$="W" and wrate<>a(1) and wrate<>0 then 
    goto READ_CUSTOMER
  else if weg$="E" and wrate<>a(3) and wrate<>0 then 
    goto READ_CUSTOMER
  else if weg$="L" and wrate<>a(3) and wrate<>0 then 
    goto READ_CUSTOMER
  else if weg$="G" and wrate<>a(4) and wrate<>0 then 
    goto READ_CUSTOMER
  else if f><d1 then 
    goto READ_CUSTOMER
  end if 
  gosub USAGE_CHART_ACCUMULATE
  if weg$="W" and d(3)>range(16) then 
    ov=min(ov+1,160)
    over(ov)=d(3)
  else if weg$="E" or weg$="L" and d(7)>range(16) then 
    ov=min(ov+1,160)
    over(ov)=d(7)
  else if weg$="G" and d(11)>range(16) then 
    ov=min(ov+1,160)
    over(ov)=d(11)
  end if 
goto READ_CUSTOMER ! /r
ENDUBM: ! r:
  close #1: 
  gosub USAGE_CHART
  gosub OVER_LIST
goto DONE ! /r
DONE: ! r:
  close #1: ioerr ignore
  fncloseprn
goto XIT ! /r
XIT: fnxit
IGNORE: continue 
USAGE_CHART_ASK_RANGE: ! r:
  fncreg_read('Per 1000 Usage - Rate Code ',weg$, 'W')
  fncreg_read('Per 1000 Usage - Service for Analysis ',wrate$, '0') : wrate=val(wrate$)
  for rangeItem=1 to 16
    fncreg_read('Per 1000 Usage - Range '&str$(rangeItem),tmp$) : range(rangeItem)=val(tmp$)
  nex rangeItem
  if sum(mat range)=0 then 
    range(1) =    0 : range(2) = 1000 : range(3) = 2000
    range(4) = 3000 : range(5) = 4000 : range(6) = 5000
    range(7) = 6000 : range(8) = 7000 : range(9) = 8000
    range(10)= 9000 : range(11)=10000 : range(12)=15000
    range(13)=20000 : range(14)=30000 : range(15)=40000
    range(16)=50000
  end if
! /r
MENU1: ! r:
  fnTos(sn$:="Per1000")
  mylen=22
  mypos=mylen+2
  respc=0
  fnLbl(2,1,"Billing Date:" ,mylen,1) : fnLbl(2,36,"(most recent billing date only)") ! ,31,0)
  fnTxt(2,mypos,8,0,1,"1")
  resp$(respc+=1)=str$(d1)
  text$="Service for Analysis:"
  fnLbl(3,1,text$,mylen,1)
  fncomboa(stfn$,3,mypos,mat opt$)
  resp$(respc+=1)=opt$(1)
  fnLbl(4,1,"Rate Code:",mylen,1)
  fnTxt(4,mypos,2,0,1,"30")
  resp$(respc+=1)="0"
  text$="Usage Break Points:"
  fnLbl(6,1,text$,mylen,1)
  for a = 1 to 16
    resp$(respc+=1) = str$(range(a))
  next a
  mypos(1)=mylen+2 : mypos(2)=mypos(1)+9
  mypos(3)=mypos(2)+9 : mypos(4)=mypos(3)+9
  fnTxt(6,mypos(1),7) : fnTxt(6,mypos(2),7)
  fnTxt(6,mypos(3),7) : fnTxt(6,mypos(4),7)
  fnTxt(7,mypos(1),7) : fnTxt(7,mypos(2),7)
  fnTxt(7,mypos(3),7) : fnTxt(7,mypos(4),7)
  fnTxt(8,mypos(1),7) : fnTxt(8,mypos(2),7)
  fnTxt(8,mypos(3),7) : fnTxt(8,mypos(4),7)
  fnTxt(9,mypos(1),7) : fnTxt(9,mypos(2),7)
  fnTxt(9,mypos(3),7) : fnTxt(9,mypos(4),7)
  fnCmdSet(3)
  fnAcs(sn$,win,mat resp$,ck)
  if ck=5 then goto XIT
  d1=val(resp$(1))
  weg$=resp$(2)(1:1)
  wrate=val(resp$(3))
  for a = 1 to 16
    range(a) = val(resp$(a+3))
  next a
  if weg$<>"W" and weg$<>"E" and weg$<>"G" and weg$<>"L" then ce=3 : goto MENU1
  fncreg_write('Per 1000 Usage - Rate Code ',weg$)
  fncreg_write('Per 1000 Usage - Service for Analysis ',str$(wrate))
  for rangeItem=1 to 16
    fncreg_write('Per 1000 Usage - Range '&str$(rangeItem),str$(range(rangeItem)))
  nex rangeItem
  if weg$="W" then 
    rtype$="Water"
  else if weg$="E" then 
    rtype$="Electric"
  else if weg$="L" then 
    rtype$="Lawn Meter"
  else if weg$="G" then 
    rtype$="Gas"
  end if 
return  ! /r
USAGE_CHART_ACCUMULATE: ! r:
  for j=1 to 15
    if weg$="W" and d(3)=>range(j) and d(3)<range(j+1) then 
      cust(j)+=1
      goto L860 
    else if weg$="E" or weg$="L" and d(7)=>range(j) and d(7)<range(j+1) then 
      cust(j)=cust(j)+1
      goto L860 
    else if weg$="G" and d(11)=>range(j) and d(11)<range(j+1) then 
      cust(j)=cust(j)+1
      goto L860
    end if
  next j
  cust(16)+=1
  x+=1 
  excess(x,1)=val(z$)
  if weg$="W" then 
    excess(x,2)=d(3) 
  else if weg$="E" or weg$="L" then 
    excess(x,2)=d(7) 
  else if weg$="G" then 
    excess(x,2)=d(11)
  end if
  L860: !
return ! /r
USAGE_CHART: ! r:
! pr #255: "{\ul                                                                                }"
  pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
  pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
  pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
  pr #255: "\qc  For the Billing Date of "&cnvrt$("PIC(##/##/##)",d1)
  if wrate<>0 then 
    pr #255: "\qc For "&rtype$&" Rate Code "&str$(wrate)
  else 
    pr #255: "\qc For "&rtype$
  end if
  pr #255: "\ql "
  pr #255,using "Form POS 7,C 18,C 30": " Usage In","  Total" 
  pr #255,using "Form POS 7,C 18,C 30": " Gallons ","Customers"
  for j=1 to 16
    if j=1 then 
      pr #255,using L990: "Under "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(2))),cust(j) 
      L990: form pos 1,cr 19,x 1,pic(zz,zzz,zz#)
    else if range(j)>10 and j<>16 then 
      pr #255,using L1000: cnvrt$("PIC(ZZZZ,ZZ#)",range(j))&" - "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(j+1)-1)),cust(j) 
      L1000: form pos 1,cr 19,x 1,pic(zz,zzz,zz#)
    else if j=16 then 
      pr #255,using L1000: cnvrt$("PIC(ZZZZ,ZZ#)",range(j))&" or more",cust(j) 
    else 
      pr #255,using L1010: range(j),cust(j)
      L1010: form pos 7,pic(zzzz,zz#),x 6,pic(zz,zzz,zz#)
    end if
  next j
  pr #255: "{\ul                                                                                }"
return ! /r
OVER_LIST: ! r:
  if over(1)<>0 then 
    pr #255: "Actual usages for customers over "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(16)))
  end if
  for j=1 to 160 step 8
    if over(j)>0 then 
      pr #255,using "form pos 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9": over(j),over(j+1),over(j+2),over(j+3),over(j+4),over(j+5),over(j+6),over(j+7) 
    else 
      goto L1110
    end if
  next j
  L1110: !
  if over(1)<>0 then 
    pr #255: "{\ul                                                                                }"
  end if
return ! /r
PGOF: pr #255: newpage : continue 

insert:ertn
