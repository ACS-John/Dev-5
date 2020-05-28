! replace S:\acsUB\sewerstats.br
! Calculates average customers billed and average sewer charges over a given time period
autoLibrary
dim resp$(2)*40,z$*10,a$*10,transkey$*19,dt(1),chg(1),ccnt(1),cycle(1,1),cyclecnt(1),avg(2,1),txt$*23
fn_main
def fn_main
	fnTop(program$,"Sewer Statistics")
	fn_getrange
	fn_openfiles
	fn_calculate
	fn_closefiles
fnend 
def fn_getrange
	fnTos("sewerstats-1")
	fnFra(1,1,3,40,"Date Range for Statistics","Enter the range of dates for which you want to calculate sewer charge statistics")
	fnLbl(1,1,"Beginning Date:",22,1,0,1)
	fnTxt(1,25,12,0,1,"3",0,"Enter the date of the first billing cycle to be included. ",1)
	resp$(1)=str$(date("mmddyy")-3)
	fnLbl(2,1,"Ending Date:",22,1,0,1)
	fnTxt(2,25,12,0,1,"3",0,"Enter the date of the last billing cycle to be included. ",1)
	resp$(2)=str$(date("mmddyy"))
	fnCmdKey("Next",1,1,0,"Calculate sewer statistics.")
	fnCmdKey("Cancel",5,0,1,"Returns to menu.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then let fnXit
	d1=val(resp$(1)) : d2=val(resp$(2)) : yrend=d1+10000 : yrcnt=1
	do while yrend<d2
		mat dt(yrcnt)=dt
		dt(yrcnt)=yrend
		yrend+=10000 : yrcnt+=1
	loop 
	mat dt(yrcnt)=dt : dt(yrcnt)=d2 : mat chg(yrcnt) : mat ccnt(yrcnt) : mat cycle(yrcnt,1) : mat cyclecnt(yrcnt) : mat avg(2,yrcnt+1)
fnend 
def fn_calculate
	totcust=lrec(h_ubmstr) : custidx=0
	do 
		NEXTCUST: !
		read #h_ubmstr,using CUSTFORM: z$ eof ENDOFCUST
		custidx+=1 : pr f "10,10,C 35": "Reading customer "&str$(custidx)&" of "&str$(totcust)
		transkey$=z$&cnvrt$("N 8",0)&cnvrt$("N 1",0)
		read #h_ubtrans,using TRANSFORM,key>=transkey$: a$,tdate,tcode,seweramt nokey NEXTCUST : goto PROCESSTRANS
		NEXTTRANS: !
		read #h_ubtrans,using TRANSFORM: a$,tdate,tcode,seweramt eof NEXTCUST
		PROCESSTRANS: ! 
		if z$=a$ then ! this is the correct customer
			if tdate>=d1 and tdate<=d2 and seweramt>0 and tcode=1 then ! include this transaction in the average
				for j = 1 to yrcnt
					if tdate<=dt(j) then 
						for k = 1 to udim(cycle,2)
							if cycle(j,k)=0 then 
								cycle(j,k)=tdate : cyclecnt(j)+=1 : goto TOT
							else 
								if tdate=cycle(j,k) then goto TOT
							end if 
						next k
						mat cycle(yrcnt,k)=cycle : cycle(j,k)=tdate : cyclecnt(j)+=1
						TOT: !
						chg(j)+=seweramt : ccnt(j)+=1 : goto NEXTTRANS
					end if 
				next j
			end if 
			goto NEXTTRANS
		else 
			goto NEXTCUST
		end if 
	loop 
	ENDOFCUST: !
	for j = 1 to yrcnt
		avg(1,j)=int(ccnt(j)/cyclecnt(j)) : avg(2,j)=round(chg(j)/ccnt(j),2)
	next j
	avg(1,yrcnt+1)=int(sum(ccnt)/sum(cyclecnt)) : avg(2,yrcnt+1)=round(sum(chg)/sum(ccnt),2)
	fn_showresults
fnend 
def fn_showresults
	lncnt=2+yrcnt
	fnTop("S:\acsUB\sewerstats2","Sewer Statistics")
	fnTos("sewerstats-2")
	fnLbl(1,1,"Date Range:",19)
	fnLbl(1,22,"Avg. Bills Per Cycle:",23)
	fnLbl(1,47,"Avg. Bill:",12)
	open #(h_prn:=fngethandle): "Name=[Q]\UBmstr\Sewerstats"&wsid$&".txt,Replace,RecL=5000",display,output 
	pr #h_prn: 'Call Print.MyOrientation("Portrait")'
	pr #h_prn: 'Call Print.MyFontSize(14)'
	pr #h_prn: 'Call Print.MyFontBold(True)'
	pr #h_prn: 'Call Print.AddText("Sewer Statistics from '&str$(d1)&' to '&str$(d2)&'",20,10)'
	pr #h_prn: 'Call Print.MyFontSize(10)'
	pr #h_prn: 'Call Print.AddText("Date Range:",10,20)'
	pr #h_prn: 'Call Print.AddText("Avg. Bills Per Cycle",50,20)'
	pr #h_prn: 'Call Print.AddText("Avg. Bill",120,20)'
	pr #h_prn: 'Call Print.MyFontBold(False)'
	for j = 1 to udim(avg,2)
		if j=1 then dtstart=d1 else dtstart=dt(j-1)+1
		if j=udim(avg,2) then txt$="Total Avg" else txt$=str$(dtstart)&"-"&str$(dt(j))
		fnLbl(j+1,1,txt$,19)
		fnLbl(j+1,22,str$(avg(1,j)),23)
		fnLbl(j+1,47,str$(avg(2,j)),12)
		pr #h_prn: 'Call Print.AddText("'&txt$&'",10,'&str$(20+j*7)&')'
		pr #h_prn: 'Call Print.AddText("'&str$(avg(1,j))&'",50,'&str$(20+j*7)&')'
		pr #h_prn: 'Call Print.AddText("$'&trim$(cnvrt$("NZ 10.2",avg(2,j)))&'",120,'&str$(20+j*7)&')'
	next j
	fnCmdKey("Print",2,0,0,"Print these statistics.")
	fnCmdKey("Done",1,1,0,"Return to menu.")
	fnAcs("sewerstats-1",0,mat resp$,ckey)
	pr #h_prn: 'Call Print.EndDoc'
	if ckey=2 then execute 'System -W -C "'&os_filename$("S:\Core\PrAce.exe")&'" '&os_filename$('UBmstr\Sewerstats"&wsid$&".txt"') ! "sy -W -C S:\Core\PrAce UBmstr\Sewerstats"&wsid$&".txt"
fnend 
def fn_openfiles
	open #(h_ubmstr:=fngethandle): "Name=[Q]\UBmstr\customer.h[cno],KFName=[Q]\UBmstr\UBIndex.h[cno]",internal,input,keyed 
	CUSTFORM: form c 10
	open #(h_ubtrans:=fngethandle): "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno]",internal,input,keyed 
	TRANSFORM: form c 10,n 8,n 1,pos 28,pd 4.2
fnend 
def fn_closefiles
	close #h_ubmstr: 
	close #h_ubtrans: 
	fnXit
fnend 
