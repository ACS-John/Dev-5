! formerly S:\acsPR\newprRptS1
! Print Custom Reports
execute 'clear proc only'
fn_setup
fnTop(program$,'Print Custom Reports')
gosub OpenReportLayout
do ! r: menu1
	fnTos
	respc=0
	fnLbl(1,1,'Report:',11,1)
	fnComboF('Report',1,14,43,'[Q]\PRmstr\prreport.h[cno]',1,2,3,30,'[Q]\PRmstr\prrptidx.h[cno]',1+addall,1,'Select from the list of reports. You can only select one report at a time.',0)
	resp$(respc+=1)=''
	fnCmdKey('&Print',1,1,0,'Prints the selected Custom Report' )
	fnCmdKey('Exit',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$) ! ask report #
	if ckey=5 then goto Xit
	reportSelectedN=val(resp$(1)(1:2))
	fn_printCustomReport(reportSelectedN,3)
	gosub OpenReportLayout
loop ! /r
Xit: fnXit
OpenReportLayout: ! r:
	close #3: ioerr ignore
	open #3: 'Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\PRRptIdx.h[cno],Shr',i,i,k ioerr NoPrReport
	if lrec(3)=0 then goto NoPrReport
	close #3:
return
	NoPrReport: ! r:
		mat ml$(2)
		ml$(1)='No reports have been designed in the Custom'
		ml$(2)='Reports file.  Click OK to go there now.'
		fnMsgBox(mat ml$,resp$,'',48)
	fnChain('S:\Payroll\Custom Reports') ! /r
! /r

def library fnPrintCustomReport(reportSelectedN)
		fn_setup
		fnPrintCustomReport=fn_printCustomReport(reportSelectedN)
fnend
def fn_printCustomReport(reportSelectedN; ___,reportSelected$*2,rt$*78)
	reportSelected$=lpad$(str$(reportSelectedN),2)
	open #hReports=fnH: 'Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\PRRptIdx.h[cno],Shr',i,i,k ! ioerr PdrXit
	read #hReports,using 'form pos 3,c 78',key=reportSelected$: rt$ nokey PcrReportsNoKey
	dim ch$(2)*132
	dim psc(100)
	dim a(20)
	dim pp(20)
	dim ti(20)
	read #hReports,using 'form pos 3,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1',key=reportSelected$: rt$,mat ch$,ips,tdep,cp,mat psc,mat a,mat pp,mat ti nokey PcrReportsNoKey
	close #hReports:
	if sum(a)=0 then
		mat ml$(2)
		ml$(1)='The report you have selected has no fields selected to print.'
		ml$(2)='Click OK to contine.'
		fnMsgBox(mat ml$,resp$,'',48)
		goto PdrXit
	end if
	dim tmpCustomReport$*512
	tmpCustomReport$='[temp]\Custom Report - s[Session].brs' ! '[Q]\PRmstr\Tmp_Custom_Report-[Session]-brs.h[cno]'
	fnCopy('S:\Payroll\Custom Report Template.brs',tmpCustomReport$)

	open #hBrs=fnH: 'Name='&tmpCustomReport$&',RecL=255',d,o
	pr #hBrs,using Fc: '00081 rn$="'&reportSelected$&'"'
	dim l19900$*255
	l19900$='19900 pr #255, using F_Pr_Out: '
	dim l20010$*255
	l20010$='20010 pr #255, using F_Pr_Out: '
	dim lineFprOut$*255
	lineFprOut$='19910 F_Pr_Out: form'
	dim gpf$*255
	gpf$='21000 pr #255, using 21010: '
	dim gaf$*255
	gaf$='21010 form skip 2,"Totals"'
	for j=1 to 20
		! if a(j) then
			if ~a(j) then goto NextJ

			if a(j)<30 then
				l19900$=rtrm$(l19900$)&ty$(a(j),1)
				l20010$=rtrm$(l20010$)&ty$(a(j),1)
			else
				! l19900$=RTRM$(l19900$)&TY$(A(J),1)&','
				l19900$=rtrm$(l19900$)&ty$(a(j),1)&','
				l20010$=rtrm$(l20010$)&ty$(a(j),4)&','
			end if
			lineFprOut$=rtrm$(lineFprOut$)&',pos '&str$(pp(j))&ty$(a(j),3)
			if tdep=1 and a(j)>29 then goto L1660

			if ti(j)<>1 then goto NextJ

			L1660: !
			lf1=len(rtrm$(ty$(a(j),1)))-1
			lf2=len(rtrm$(ty$(a(j),4)))
			dim datLine$*512
			! exe 'break datLine$' ! XXX
			if ti(j)=1 then
				dim numberGroupA$*128
				numberGroupA$=',2,3,4,5,6,26,27,62,63,'
				dim aj$
				aj$=','&str$(a(j))&','
				if pos(numberGroupA$,aj$)>0 then ! if a(j)=2 or a(j)=3 or a(j)=4 or a(j)=5 or a(j)=6 or a(j)=26 or a(j)=27 or a(j)=62 or a(j)=63 then
					goto L1720
				end if
				datLine$=str$(19910+j)&' '&ty$(a(j),4)&'='&ty$(a(j),4)&'+'&ty$(a(j),1)(1:lf1)
				if pos(datLine$,',,')>0 then pr ',, detected.' : pause 
				pr #hBrs,using Fc: datLine$
			end if
			if pos(numberGroupA$,aj$)>0 then ! if a(j)=2 or a(j)=3 or a(j)=4 or a(j)=5 or a(j)=6 or a(j)=26 or a(j)=27 or a(j)=62 or a(j)=63 then
				goto L1720
			end if
			datLine$=str$(19930+j)&' '&ty$(a(j),5)&'='&ty$(a(j),5)&'+'&ty$(a(j),1)(1:lf1)
			pr #hBrs,using Fc: datLine$

			L1720: !
			if ti(j)<>1 then goto NextJ
			if pos(numberGroupA$,aj$)>0 then ! if a(j)=2 or a(j)=3 or a(j)=4 or a(j)=5 or a(j)=6 or a(j)=26 or a(j)=27 or a(j)=62 or a(j)=63 then
				goto L1760
			end if

			gaf$=rtrm$(gaf$)&',pos '&str$(pp(j))&ty$(a(j),3)
			gpf$=rtrm$(gpf$)&ty$(a(j),5)&','

			L1760: !
			ti1=1
		! end if ! a(j)
		NextJ: !
	next j
	lineFprOut$=srep$(lineFprOut$,'form,','form ') ! lineFprOut$(11:11)=' '
	lf1=len(rtrm$(l19900$))
	lf2=len(rtrm$(l20010$))
	! if l19900$(1:5)='19900' then pr l19900$ : pause
	! if l20010$(1:5)='20010' then pr l20010$ : pause
	if l19900$(lf1:lf1)=':' then l19900$&='"",' : lf1+=3
	if l20010$(lf1:lf1)=':' then l20010$&='"",' : lf1+=3
	l19900$(lf1:lf1)=' '
	l20010$(lf2:lf2)=' '
	
l19900$=srep$(l19900$,',,',',')   ! seems to be a bandaid for something i messed up earlier in the logic 2/1/23 jb
l19900$=rtrm$(rtrm$(l19900$),',') ! seems to be a bandaid for something i messed up earlier in the logic 2/1/23 jb
! pr l19900$ : pause
	l19900$=rtrm$(l19900$)&' pageoflow PgOf'
	l20010$=rtrm$(l20010$)&' pageoflow PgOf'
	lf1=len(rtrm$(gpf$))
	gpf$(lf1:lf1)=' '
	gpf$=rtrm$(gpf$)&' pageoflow PgOf'
	pr #hBrs,using Fc: ''
	Fc: form pos 1,c 255
	! pr #hBrs,Using 2050: AF1$
	! pr #hBrs,Using 2050: AF2$
	! pr #hBrs,Using 2240: RF2$
	pr #hBrs,using Fc: l19900$
	pr #hBrs,using Fc: lineFprOut$
	pr #hBrs,using Fc: '20099 goto PRTRPT' ! L20099:
	pr #hBrs,using Fc: '20000 L20000: if tdep=0 then goto PRTRPT'
	pr #hBrs,using Fc: '20010 L20010: '&l20010$(7:255)
	pr #hBrs,using Fc: '20020 L20020: mat dt=(0)'
	pr #hBrs,using Fc: '19990 goto L19804'
	if ti1 then
		pr #hBrs,using Fc: gpf$
		pr #hBrs,using Fc: gaf$
	end if
	pr #hBrs,using Fc: '21200 goto EOF1'
	pr #hBrs,using Fc: '20100 ! Check for Totals to pr ______________'
	pr #hBrs,using Fc: '19899 L19899: if tdep=1 then goto F_Pr_Out'
	if ips=0 then goto L20001
	lf1=len(rtrm$(ty$(ips,1)))-1 : lf2=len(rtrm$(ty$(ips,4)))-1
	! pr #hBrs,using Fc: '19801 ipsw=0'
	if ips>24 and ips<104 then
		goto L19832
	else
		pr #hBrs,using Fc: '19811 For j=1 to 100'
		pr #hBrs,using Fc: '19812 if psc(1)=-1 and TY$(IPS,1)(1:LF1)<>'''' then goto L19817'
		pr #hBrs,using Fc: '19814   if '&ty$(ips,1)(1:lf1)&'=psc(j) then goto L19817'
		pr #hBrs,using Fc: '19813   if psc(j)=0 then goto L19804'
		pr #hBrs,using Fc: '19815 next j'
		pr #hBrs,using Fc: '19816 goto L19804'
		pr #hBrs,using Fc: '19817 L19817: ipsw=1'
		goto L20001
	end if
	L19832: !
		pr #hBrs,using Fc: '19832 for j=1 to 100'
		pr #hBrs,using Fc: '19833   if psc(j)=0  or (psc(1)=-1 and ty$(ips,1)(1:lf1)<>'''') then goto L19836'
		pr #hBrs,using Fc: '19834   if '&ty$(ips,1)(1:lf1)&'= psc(j) then goto L19838'
		pr #hBrs,using Fc: '19835 next j'
		pr #hBrs,using Fc: '19836 L19836: if ipsw=0 then ipsw=9'
		pr #hBrs,using Fc: '19837 goto L19804'
		pr #hBrs,using Fc: '19838 L19838: ipsw=1'
	L20001: !
		pr #hBrs,using Fc: '20001 if ips=0 or ipsw=1 then goto L20010 else goto L20020'
		close #hBrs:
		dim procName$*256
		procName$='[Temp]\Custom Report - s[session].proc'
		fnWriteProc(procName$	,'Load "'&tmpCustomReport$&'",Source')
		fnWriteProc(''       	,'Run')
		! pr 'just before proc='&procName$ : pause

	chain 'Proc='&procName$

	PdrXit: !
	! pr 'encountered PdrXit' : pause
fnend
	PcrReportsNoKey: ! r: no records on file
	! if reportSelectedN=0 then goto Xit
		mat ml$(2)
		ml$(1)='Custom Report ('&reportSelected$&') could not be found for printing.'
		ml$(2)='Click OK to contine.'
		fnMsgBox(mat ml$,resp$,'',48)
	goto PdrXit ! /r

def fn_setup
	autoLibrary
	on error goto Ertn
	dim ml$(0)*128
	dim resp$(40)*200
	! a$*255,a$(20)*32,iom$(20),af1$*255,af2$*255,af1$*255,af2$*255,msgline$(2)*60,t(104),tt(104),e$(7)*30,e(17),ansm(20),wrdm$(20)*65,rf2$*255,message$*40,

	! r: set mat ty$
		ty$(  1,1)='eno,'       	: ty$(  1,2)='pos   1,n  8,'   	: ty$(  1,3)=',pic(zzzzzzzz)'      	: ty$(  1,4)=  'dt(2)'	: ty$(  1,5)=  'GT(2)'
		ty$(  2,1)='em$(1),'    	: ty$(  2,2)='pos   9,c 30,'   	: ty$(  2,3)=',c 30'               	: ty$(  2,4)=  'dt(3)'	: ty$(  2,5)=  'GT(3)'
		ty$(  3,1)='em$(2),'    	: ty$(  3,2)='pos  39,c 30,'   	: ty$(  3,3)=',c 30'               	: ty$(  3,4)=  'dt(4)'	: ty$(  3,5)=  'GT(4)'
		ty$(  4,1)='em$(3),'    	: ty$(  4,2)='pos  69,c 30,'   	: ty$(  4,3)=',c 30'               	: ty$(  4,4)=  'dt(5)'	: ty$(  4,5)=  'GT(5)'
		ty$(  5,1)='ss$,'       	: ty$(  5,2)='pos  99,c 11,'   	: ty$(  5,3)=',c 11'               	: ty$(  5,4)=  'dt(6)'	: ty$(  5,5)=  'GT(6)'
		ty$(  6,1)='rs(1),'     	: ty$(  6,2)='pos 110,n 1,'    	: ty$(  6,3)=',n 1'                	: ty$(  6,4)=  'dt(7)'	: ty$(  6,5)=  'GT(7)'
		ty$(  7,1)='rs(2),'    	: ty$(  7,2)='pos 111,n 1,'    	: ty$(  7,3)=',n 1'                	: ty$(  7,4)=  'dt(8)'	: ty$(  7,5)=  'GT(8)'
		ty$(  8,1)='em(1),'    	: ty$(  8,2)='pos 112,n 2,'    	: ty$(  8,3)=',pic(zz)'            	: ty$(  8,4)=  'dt(9)'	: ty$(  8,5)=  'GT(9)'
		ty$(  9,1)='em(2),'    	: ty$(  9,2)='pos 114,n 2,'    	: ty$(  9,3)=',pic(zz)'            	: ty$(  9,4)= 'dt(10)'	: ty$(  9,5)= 'GT(10)'
		ty$( 10,1)='em(3),'    	: ty$( 10,2)='pos 116,n 2,'    	: ty$( 10,3)=',pic(zz)'            	: ty$( 10,4)= 'dt(11)'	: ty$( 10,5)= 'GT(11)'
		ty$( 11,1)='em(4),'    	: ty$( 11,2)='pos 118,n 2,'    	: ty$( 11,3)=',pic(zz)'            	: ty$( 11,4)= 'dt(12)'	: ty$( 11,5)= 'GT(12)'
		ty$( 12,1)='em(5),'    	: ty$( 12,2)='pos 120,n 2,'    	: ty$( 12,3)=',pic(zz)'            	: ty$( 12,4)= 'dt(13)'	: ty$( 12,5)= 'GT(13)'
		ty$( 13,1)='em(6),'    	: ty$( 13,2)='pos 122,n 2,'    	: ty$( 13,3)=',pic(zz)'            	: ty$( 13,4)= 'dt(14)'	: ty$( 13,5)= 'GT(14)'
		ty$( 14,1)='em(7),'    	: ty$( 14,2)='pos 124,n 2,'    	: ty$( 14,3)=',pic(zz)'            	: ty$( 14,4)= 'dt(15)'	: ty$( 14,5)= 'GT(15)'
		ty$( 15,1)='em(8),'    	: ty$( 15,2)='pos 126,pd 3.3,'	: ty$( 15,3)=',pic(---.###)'       	: ty$( 15,4)= 'dt(16)'	: ty$( 15,5)= 'GT(16)'
		ty$( 16,1)='em(9),'    	: ty$( 16,2)='pos 129,pd 3.3,'	: ty$( 16,3)=',pic(---.###)'       	: ty$( 16,4)= 'dt(17)'	: ty$( 16,5)= 'GT(17)'
		ty$( 17,1)='em(10),'   	: ty$( 17,2)='pos 132,pd 4.2,'	: ty$( 17,3)=',pic(--,---.##)'    	: ty$( 17,4)= 'dt(18)'	: ty$( 17,5)= 'GT(18)'
		ty$( 18,1)='em(11),'   	: ty$( 18,2)='pos 136,pd 4.2,'	: ty$( 18,3)=',pic(--,---.##)'    	: ty$( 18,4)= 'dt(19)'	: ty$( 18,5)= 'GT(19)'
		ty$( 19,1)='em(12),'   	: ty$( 19,2)='pos 140,pd 4.2,'	: ty$( 19,3)=',pic(--,---.##)'    	: ty$( 19,4)= 'dt(20)'	: ty$( 19,5)= 'GT(20)'
		ty$( 20,1)='em(13),'   	: ty$( 20,2)='pos 144,pd 4.2,'	: ty$( 20,3)=',pic(--,---.##)'    	: ty$( 20,4)= 'dt(21)'	: ty$( 20,5)= 'GT(21)'
		ty$( 21,1)='em(14),'   	: ty$( 21,2)='pos 148,pd 4.2,'	: ty$( 21,3)=',pic(--,---.##)'    	: ty$( 21,4)= 'dt(22)'	: ty$( 21,5)= 'GT(22)'
		ty$( 22,1)='em(15),'   	: ty$( 22,2)='pos 152,pd 4.2,'	: ty$( 22,3)=',pic(--,---.##)'    	: ty$( 22,4)= 'dt(23)'	: ty$( 22,5)= 'GT(23)'
		ty$( 23,1)='em(16),'   	: ty$( 23,2)='pos 156,n 6,'    	: ty$( 23,3)=',pic(zz/zz/zz)'      	: ty$( 23,4)= 'dt(24)'	: ty$( 23,5)= 'GT(24)'
		ty$( 24,1)='lpd,'      	: ty$( 24,2)='pos 162,n 6,'    	: ty$( 24,3)=',pic(zz/zz/zz)'      	: ty$( 24,4)= 'dt(25)'	: ty$( 24,5)= 'GT(25)'
		ty$( 25,1)='ph$,'      	: ty$( 25,2)='pos 168,c 12,'   	: ty$( 25,3)=',c 12'               	: ty$( 25,4)= 'dt(26)'	: ty$( 25,5)= 'GT(26)'
		ty$( 26,1)='bd,'        	: ty$( 26,2)='pos 180,n 6,'    	: ty$( 26,3)=',pic(zz/zz/zz)'      	: ty$( 26,4)= 'dt(27)'	: ty$( 26,5)= 'GT(27)'
		ty$( 27,1)='tdn,'      	: ty$( 27,2)='pos   9,n 3,'    	: ty$( 27,3)=',n 3'                	: ty$( 27,4)= 'dt(28)'	: ty$( 27,5)= 'GT(28)'
		ty$( 28,1)='tgl2,'     	: ty$( 28,2)='pos  15,n 6,'    	: ty$( 28,3)=',n 6'                	: ty$( 28,4)= 'dt(29)'	: ty$( 28,5)= 'GT(29)'
		ty$( 29,1)='tdt(1),'   	: ty$( 29,2)='pos  24,n 6,'    	: ty$( 29,3)=',pic(zz/zz/zz)'      	: ty$( 29,4)= 'dt(30)'	: ty$( 29,5)= 'GT(30)'
		ty$( 30,1)='tdt(2),'   	: ty$( 30,2)='pos  30,n 6,'    	: ty$( 30,3)=',pic(zz/zz/zz)'      	: ty$( 30,4)= 'dt(31)'	: ty$( 30,5)= 'GT(31)'
		ty$( 31,1)='tdt(3),'   	: ty$( 31,2)='pos  36,n 6,'    	: ty$( 31,3)=',pic(zz/zz/zz)'      	: ty$( 31,4)= 'dt(32)'	: ty$( 31,5)= 'GT(32)'
		ty$( 32,1)='tdt(4),'   	: ty$( 32,2)='pos  42,n 6,'    	: ty$( 32,3)=',pic(zz/zz/zz)'      	: ty$( 32,4)= 'dt(33)'	: ty$( 32,5)= 'GT(33)'
		ty$( 33,1)='tcd(1),'   	: ty$( 33,2)='pos  48,n 2,'    	: ty$( 33,3)=',pic(zz)'            	: ty$( 33,4)= 'dt(34)'	: ty$( 33,5)= 'GT(34)'
		ty$( 34,1)='tcd(2),'   	: ty$( 34,2)='pos  50,n 2,'    	: ty$( 34,3)=',pic(zz)'            	: ty$( 34,4)= 'dt(35)'	: ty$( 34,5)= 'GT(35)'
		ty$( 35,1)='tcd(3),'   	: ty$( 35,2)='pos  52,n 2,'    	: ty$( 35,3)=',pic(zz)'            	: ty$( 35,4)= 'dt(36)'	: ty$( 35,5)= 'GT(36)'
		ty$( 36,1)='tli,'      	: ty$( 36,2)='pos  54,pd 4.2,' 	: ty$( 36,3)=',pic(---,---.##)'   	: ty$( 36,4)= 'dt(37)'	: ty$( 36,5)= 'GT(37)'
		ty$( 37,1)='tdet(1),'  	: ty$( 37,2)='pos  58,pd 4.2,' 	: ty$( 37,3)=',pic(zz,zzz.##)'    	: ty$( 37,4)= 'dt(38)'	: ty$( 37,5)= 'GT(38)'
		ty$( 38,1)='tdet(2),'  	: ty$( 38,2)='pos  62,pd 4.2,' 	: ty$( 38,3)=',pic(zz,zzz.##)'    	: ty$( 38,4)= 'dt(39)'	: ty$( 38,5)= 'GT(39)'
		ty$( 39,1)='tdet(3),'  	: ty$( 39,2)='pos  66,pd 4.2,' 	: ty$( 39,3)=',pic(zz,zzz.##)'    	: ty$( 39,4)= 'dt(40)'	: ty$( 39,5)= 'GT(40)'
		ty$( 40,1)='tdet(4),'  	: ty$( 40,2)='pos  70,pd 4.2,' 	: ty$( 40,3)=',pic(---,---.##)'   	: ty$( 40,4)= 'dt(41)'	: ty$( 40,5)= 'GT(41)'
		ty$( 41,1)='tdet(5),'  	: ty$( 41,2)='pos  74,pd 4.2,' 	: ty$( 41,3)=',pic(---,---.##)'   	: ty$( 41,4)= 'dt(42)'	: ty$( 41,5)= 'GT(42)'
		ty$( 42,1)='tdet(6),'  	: ty$( 42,2)='pos  78,pd 4.2,' 	: ty$( 42,3)=',pic(---,---.##)'   	: ty$( 42,4)= 'dt(43)'	: ty$( 42,5)= 'GT(43)'
		ty$( 43,1)='tdet(7),'  	: ty$( 43,2)='pos  82,pd 4.2,' 	: ty$( 43,3)=',pic(---,---.##)'   	: ty$( 43,4)= 'dt(44)'	: ty$( 43,5)= 'GT(44)'
		ty$( 44,1)='tdet(8),'  	: ty$( 44,2)='pos  86,pd 4.2,' 	: ty$( 44,3)=',pic(---,---.##)'   	: ty$( 44,4)= 'dt(45)'	: ty$( 44,5)= 'GT(45)'
		ty$( 45,1)='tdet(9),'  	: ty$( 45,2)='pos  90,pd 4.2,' 	: ty$( 45,3)=',pic(---,---.##)'   	: ty$( 45,4)= 'dt(46)'	: ty$( 45,5)= 'GT(46)'
		ty$( 46,1)='tdet(10),' 	: ty$( 46,2)='pos  94,pd 4.2,' 	: ty$( 46,3)=',pic(---,---.##)'   	: ty$( 46,4)= 'dt(47)'	: ty$( 46,5)= 'GT(47)'
		ty$( 47,1)='tdet(11),' 	: ty$( 47,2)='pos  98,pd 4.2,' 	: ty$( 47,3)=',pic(---,---.##)'   	: ty$( 47,4)= 'dt(48)'	: ty$( 47,5)= 'GT(48)'
		ty$( 48,1)='tdet(12),' 	: ty$( 48,2)='pos 102,pd 4.2,'	: ty$( 48,3)=',pic(---,---.##)'   	: ty$( 48,4)= 'dt(49)'	: ty$( 48,5)= 'GT(49)'
		ty$( 49,1)='tdet(13),' 	: ty$( 49,2)='pos 106,pd 4.2,'	: ty$( 49,3)=',pic(---,---.##)'   	: ty$( 49,4)= 'dt(50)'	: ty$( 49,5)= 'GT(50)'
		ty$( 50,1)='tdet(14),' 	: ty$( 50,2)='pos 110,pd 4.2,'	: ty$( 50,3)=',pic(---,---.##)'   	: ty$( 50,4)= 'dt(51)'	: ty$( 50,5)= 'GT(51)'
		ty$( 51,1)='tdet(15),' 	: ty$( 51,2)='pos 114,pd 4.2,'	: ty$( 51,3)=',pic(---,---.##)'   	: ty$( 51,4)= 'dt(52)'	: ty$( 51,5)= 'GT(52)'
		ty$( 52,1)='tdet(16),' 	: ty$( 52,2)='pos 118,pd 4.2,'	: ty$( 52,3)=',pic(---,---.##)'   	: ty$( 52,4)= 'dt(53)'	: ty$( 52,5)= 'GT(53)'
		ty$( 53,1)='tdet(17),' 	: ty$( 53,2)='pos 122,pd 4.2,'	: ty$( 53,3)=',pic(---,---.##)'   	: ty$( 53,4)= 'dt(54)'	: ty$( 53,5)= 'GT(54)'
		ty$( 54,1)='tdet(18),' 	: ty$( 54,2)='pos 126,pd 4.2,'	: ty$( 54,3)=',pic(---,---.##)'   	: ty$( 54,4)= 'dt(55)'	: ty$( 54,5)= 'GT(55)'
		ty$( 55,1)='tdet(19),' 	: ty$( 55,2)='pos 130,pd 4.2,'	: ty$( 55,3)=',pic(---,---.##)'   	: ty$( 55,4)= 'dt(56)'	: ty$( 55,5)= 'GT(56)'
		ty$( 56,1)='tdet(20),' 	: ty$( 56,2)='pos 134,pd 4.2,'	: ty$( 56,3)=',pic(---,---.##)'   	: ty$( 56,4)= 'dt(57)'	: ty$( 56,5)= 'GT(57)'
		ty$( 57,1)='tdet(21),' 	: ty$( 57,2)='pos 138,pd 4.2,'	: ty$( 57,3)=',pic(---,---.##)'   	: ty$( 57,4)= 'dt(58)'	: ty$( 57,5)= 'GT(58)'
		ty$( 58,1)='tdet(22),' 	: ty$( 58,2)='pos 142,pd 4.2,'	: ty$( 58,3)=',pic(---,---.##)'   	: ty$( 58,4)= 'dt(59)'	: ty$( 58,5)= 'GT(59)'
		ty$( 59,1)='tdet(23),' 	: ty$( 59,2)='pos 146,pd 4.2,'	: ty$( 59,3)=',pic(---,---.##)'   	: ty$( 59,4)= 'dt(60)'	: ty$( 59,5)= 'GT(60)'
		ty$( 60,1)='tdn,'      	: ty$( 60,2)='pos   9,n 3,'    	: ty$( 60,3)=',pic(-###)'          	: ty$( 60,4)= 'dt(61)'	: ty$( 60,5)= 'GT(61)'
		ty$( 61,1)='prd,'      	: ty$( 61,2)='pos  12,pd 6,'   	: ty$( 61,3)=',pic(zzzz/zz/zz)'   	: ty$( 61,4)= 'dt(62)'	: ty$( 61,5)= 'GT(62)'
		ty$( 62,1)='ckno,'     	: ty$( 62,2)='pos  18,n 7,'    	: ty$( 62,3)=',pic(zzzzzzz)'       	: ty$( 62,4)= 'dt(63)'	: ty$( 62,5)= 'GT(63)'
		ty$( 63,1)='tdc(1),'   	: ty$( 63,2)='pos  25,pd 3.2,' 	: ty$( 63,3)=',pic(---,---.##)'   	: ty$( 63,4)= 'dt(64)'	: ty$( 63,5)= 'GT(64)'
		ty$( 64,1)='tdc(2),'   	: ty$( 64,2)='pos  28,pd 3.2,' 	: ty$( 64,3)=',pic(---,---.##)'   	: ty$( 64,4)= 'dt(65)'	: ty$( 64,5)= 'GT(65)'
		ty$( 65,1)='tdc(3),'   	: ty$( 65,2)='pos  31,pd 3.2,' 	: ty$( 65,3)=',pic(---,---.##)'   	: ty$( 65,4)= 'dt(66)'	: ty$( 65,5)= 'GT(66)'
		ty$( 66,1)='tdc(4),'   	: ty$( 66,2)='pos  34,pd 3.2,' 	: ty$( 66,3)=',pic(---,---.##)'   	: ty$( 66,4)= 'dt(67)'	: ty$( 66,5)= 'GT(67)'
		ty$( 67,1)='tdc(5),'   	: ty$( 67,2)='pos  37,pd 3.2,' 	: ty$( 67,3)=',pic(---,---.##)'   	: ty$( 67,4)= 'dt(68)'	: ty$( 67,5)= 'GT(68)'
		ty$( 68,1)='tdc(6),'   	: ty$( 68,2)='pos  40,pd 3.2,' 	: ty$( 68,3)=',pic(-,---.##)'      	: ty$( 68,4)= 'dt(69)'	: ty$( 68,5)= 'GT(69)'
		ty$( 69,1)='tdc(7),'   	: ty$( 69,2)='pos  45,pd 5.2,' 	: ty$( 69,3)=',pic(----,---.##)'  	: ty$( 69,4)= 'dt(70)'	: ty$( 69,5)= 'GT(70)'
		ty$( 70,1)='tdc(8),'   	: ty$( 70,2)='pos  50,pd 5.2,' 	: ty$( 70,3)=',pic(----,---.##)'  	: ty$( 70,4)= 'dt(71)'	: ty$( 70,5)= 'GT(71)'
		ty$( 71,1)='tdc(9),'   	: ty$( 71,2)='pos  55,pd 5.2,' 	: ty$( 71,3)=',pic(----,---.##)'  	: ty$( 71,4)= 'dt(72)'	: ty$( 71,5)= 'GT(72)'
		ty$( 72,1)='tdc(10),'  	: ty$( 72,2)='pos  60,pd 5.2,' 	: ty$( 72,3)=',pic(----,---.##)'  	: ty$( 72,4)= 'dt(73)'	: ty$( 72,5)= 'GT(73)'
		ty$( 73,1)='tcp(1),'   	: ty$( 73,2)='pos  65,pd 5.2,' 	: ty$( 73,3)=',pic(--,---,---.##)'	: ty$( 73,4)= 'dt(74)'	: ty$( 73,5)= 'GT(74)'
		ty$( 74,1)='tcp(2),'   	: ty$( 74,2)='pos  70,pd 5.2,' 	: ty$( 74,3)=',pic(--,---,---.##)'	: ty$( 74,4)= 'dt(75)'	: ty$( 74,5)= 'GT(75)'
		ty$( 75,1)='tcp(3),'   	: ty$( 75,2)='pos  75,pd 5.2,' 	: ty$( 75,3)=',pic(--,---,---.##)'	: ty$( 75,4)= 'dt(76)'	: ty$( 75,5)= 'GT(76)'
		ty$( 76,1)='tcp(4),'   	: ty$( 76,2)='pos  80,pd 5.2,' 	: ty$( 76,3)=',pic(--,---,---.##)'	: ty$( 76,4)= 'dt(77)'	: ty$( 76,5)= 'GT(77)'
		ty$( 77,1)='tcp(5),'   	: ty$( 77,2)='pos  85,pd 5.2,' 	: ty$( 77,3)=',pic(--,---,---.##)'	: ty$( 77,4)= 'dt(78)'	: ty$( 77,5)= 'GT(78)'
		ty$( 78,1)='tcp(6),'   	: ty$( 78,2)='pos  90,pd 5.2,' 	: ty$( 78,3)=',pic(--,---,---.##)'	: ty$( 78,4)= 'dt(79)'	: ty$( 78,5)= 'GT(79)'
		ty$( 79,1)='tcp(7),'   	: ty$( 79,2)='pos  95,pd 5.2,' 	: ty$( 79,3)=',pic(--,---,---.##)'	: ty$( 79,4)= 'dt(80)'	: ty$( 79,5)= 'GT(80)'
		ty$( 80,1)='tcp(8),'   	: ty$( 80,2)='pos 100,pd 5.2,'	: ty$( 80,3)=',pic(--,---,---.##)'	: ty$( 80,4)= 'dt(81)'	: ty$( 80,5)= 'GT(81)'
		ty$( 81,1)='tcp(9),'   	: ty$( 81,2)='pos 105,pd 5.2,'	: ty$( 81,3)=',pic(--,---,---.##)'	: ty$( 81,4)= 'dt(82)'	: ty$( 81,5)= 'GT(82)'
		ty$( 82,1)='tcp(10),'  	: ty$( 82,2)='pos 110,pd 5.2,'	: ty$( 82,3)=',pic(--,---,---.##)'	: ty$( 82,4)= 'dt(83)'	: ty$( 82,5)= 'GT(83)'
		ty$( 83,1)='tcp(11),'  	: ty$( 83,2)='pos 115,pd 5.2,'	: ty$( 83,3)=',pic(--,---,---.##)'	: ty$( 83,4)= 'dt(84)'	: ty$( 83,5)= 'GT(84)'
		ty$( 84,1)='tcp(12),'  	: ty$( 84,2)='pos 120,pd 5.2,'	: ty$( 84,3)=',pic(--,---,---.##)'	: ty$( 84,4)= 'dt(85)'	: ty$( 84,5)= 'GT(85)'
		ty$( 85,1)='tcp(13),'  	: ty$( 85,2)='pos 125,pd 5.2,'	: ty$( 85,3)=',pic(--,---,---.##)'	: ty$( 85,4)= 'dt(86)'	: ty$( 85,5)= 'GT(86)'
		ty$( 86,1)='tcp(14),'  	: ty$( 86,2)='pos 130,pd 5.2,'	: ty$( 86,3)=',pic(--,---,---.##)'	: ty$( 86,4)= 'dt(87)'	: ty$( 86,5)= 'GT(87)'
		ty$( 87,1)='tcp(15),'  	: ty$( 87,2)='pos 135,pd 5.2,'	: ty$( 87,3)=',pic(--,---,---.##)'	: ty$( 87,4)= 'dt(88)'	: ty$( 87,5)= 'GT(88)'
		ty$( 88,1)='tcp(16),'  	: ty$( 88,2)='pos 140,pd 5.2,'	: ty$( 88,3)=',pic(--,---,---.##)'	: ty$( 88,4)= 'dt(89)'	: ty$( 88,5)= 'GT(89)'
		ty$( 89,1)='tcp(17),'  	: ty$( 89,2)='pos 145,pd 5.2,'	: ty$( 89,3)=',pic(--,---,---.##)'	: ty$( 89,4)= 'dt(90)'	: ty$( 89,5)= 'GT(90)'
		ty$( 90,1)='tcp(18),'  	: ty$( 90,2)='pos 150,pd 5.2,'	: ty$( 90,3)=',pic(--,---,---.##)'	: ty$( 90,4)= 'dt(91)'	: ty$( 90,5)= 'GT(91)'
		ty$( 91,1)='tcp(19),'  	: ty$( 91,2)='pos 155,pd 5.2,'	: ty$( 91,3)=',pic(--,---,---.##)'	: ty$( 91,4)= 'dt(92)'	: ty$( 91,5)= 'GT(92)'
		ty$( 92,1)='tcp(20),'  	: ty$( 92,2)='pos 160,pd 5.2,'	: ty$( 92,3)=',pic(--,---,---.##)'	: ty$( 92,4)= 'dt(93)'	: ty$( 92,5)= 'GT(93)'
		ty$( 93,1)='tcp(21),'  	: ty$( 93,2)='pos 165,pd 5.2,'	: ty$( 93,3)=',pic(--,---,---.##)'	: ty$( 93,4)= 'dt(94)'	: ty$( 93,5)= 'GT(94)'
		ty$( 94,1)='tcp(22),'  	: ty$( 94,2)='pos 170,pd 5.2,'	: ty$( 94,3)=',pic(--,---,---.##)'	: ty$( 94,4)= 'dt(95)'	: ty$( 94,5)= 'GT(95)'
		ty$( 95,1)='tcp(23),'  	: ty$( 95,2)='pos 175,pd 5.2,'	: ty$( 95,3)=',pic(--,---,---.##)'	: ty$( 95,4)= 'dt(96)'	: ty$( 95,5)= 'GT(96)'
		ty$( 96,1)='tcp(24),'  	: ty$( 96,2)='pos 175,pd 5.2,'	: ty$( 96,3)=',pic(--,---,---.##)'	: ty$( 96,4)= 'dt(97)'	: ty$( 96,5)= 'GT(97)'
		ty$( 97,1)='tcp(25),'  	: ty$( 97,2)='pos 180,pd 5.2,'	: ty$( 97,3)=',pic(--,---,---.##)'	: ty$( 97,4)= 'dt(98)'	: ty$( 97,5)= 'GT(98)'
		ty$( 98,1)='tcp(26),'  	: ty$( 98,2)='pos 185,pd 5.2,'	: ty$( 98,3)=',pic(--,---,---.##)'	: ty$( 98,4)= 'dt(99)'	: ty$( 98,5)= 'GT(99)'
		ty$( 99,1)='tcp(27),'  	: ty$( 99,2)='pos 190,pd 5.2,'	: ty$( 99,3)=',pic(--,---,---.##)'	: ty$( 99,4)='dt(100)'	: ty$( 99,5)='GT(100)'
		ty$(100,1)='tcp(28),'  	: ty$(100,2)='pos 195,pd 5.2,'	: ty$(100,3)=',pic(--,---,---.##)'	: ty$(100,4)='dt(101)'	: ty$(100,5)='GT(101)'
		ty$(101,1)='tcp(29),'  	: ty$(101,2)='pos 200,pd 5.2,'	: ty$(101,3)=',pic(--,---,---.##)'	: ty$(101,4)='dt(102)'	: ty$(101,5)='GT(102)'
		ty$(102,1)='tcp(30),'  	: ty$(102,2)='pos 205,pd 5.2,'	: ty$(102,3)=',pic(--,---,---.##)'	: ty$(102,4)='dt(103)'	: ty$(102,5)='GT(103)'
		ty$(103,1)='tcp(31),'  	: ty$(103,2)='pos 210,pd 5.2,'	: ty$(103,3)=',pic(--,---,---.##)'	: ty$(103,4)='dt(104)'	: ty$(103,5)='GT(104)'
		ty$(104,1)='tcp(32),'  	: ty$(104,2)='pos 215,pd 5.2,'	: ty$(104,3)=',pic(--,---,---.##)'	: ty$(104,4)='dt(105)'	: ty$(104,5)='GT(105)'
		dim ty$(104,5)*20
	! /r
fnend
include: ertn