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

	fnCopy('S:\Payroll\Custom Report Template.brs','[Q]\PRmstr\Tmp_Custom_Report-[Session]-brs.h[cno]')

	open #hBrs=fnH: 'Name=[Q]\PRmstr\Tmp_Custom_Report-[Session]-brs.h[cno],RecL=255',d,o
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
		procName$='[Temp]\Launch Custom Report - [acsuserid].proc'
		fnWriteProc(procName$	,'Load "[Q]\PRmstr\Tmp_Custom_Report-[Session]-brs.h[cno]",Source')
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
		ty$(1  ,1)='ENO,'       	: ty$(1  ,2)='pos 1,n 8,'      	: ty$(1  ,3)=',pic(zzzzzzzz)'      	: ty$(1  ,4)=  'DT(2)'	: ty$(1  ,5)=  'GT(2)'
		ty$(2  ,1)='EM$(1),'    	: ty$(2  ,2)='pos 9,C 30,'     	: ty$(2  ,3)=',c 30'               	: ty$(2  ,4)=  'DT(3)'	: ty$(2  ,5)=  'GT(3)'
		ty$(3  ,1)='EM$(2),'    	: ty$(3  ,2)='pos 39,C 30,'    	: ty$(3  ,3)=',c 30'               	: ty$(3  ,4)=  'DT(4)'	: ty$(3  ,5)=  'GT(4)'
		ty$(4  ,1)='EM$(3),'    	: ty$(4  ,2)='pos 69,C 30,'    	: ty$(4  ,3)=',c 30'               	: ty$(4  ,4)=  'DT(5)'	: ty$(4  ,5)=  'GT(5)'
		ty$(5  ,1)='SS$,'       	: ty$(5  ,2)='pos 99,C 11,'    	: ty$(5  ,3)=',c 11'               	: ty$(5  ,4)=  'DT(6)'	: ty$(5  ,5)=  'GT(6)'
		ty$(6  ,1)='RS(1),'     	: ty$(6  ,2)='pos 110,n 1,'    	: ty$(6  ,3)=',N 1'                	: ty$(6  ,4)=  'DT(7)'	: ty$(6  ,5)=  'GT(7)'
		ty$(7  ,1)='RS(2),'    	: ty$(7  ,2)='pos 111,n 1,'    	: ty$(7  ,3)=',N 1'                	: ty$(7  ,4)=  'DT(8)'	: ty$(7  ,5)=  'GT(8)'
		ty$(8  ,1)='EM(1),'    	: ty$(8  ,2)='pos 112,N 2,'    	: ty$(8  ,3)=',pic(zz)'            	: ty$(8  ,4)=  'DT(9)'	: ty$(8  ,5)=  'GT(9)'
		ty$(9  ,1)='EM(2),'    	: ty$(9  ,2)='pos 114,N 2,'    	: ty$(9  ,3)=',pic(zz)'            	: ty$(9  ,4)= 'DT(10)'	: ty$(9  ,5)= 'GT(10)'
		ty$(10 ,1)='EM(3),'    	: ty$(10 ,2)='pos 116,N 2,'    	: ty$(10 ,3)=',pic(zz)'            	: ty$(10 ,4)= 'DT(11)'	: ty$(10 ,5)= 'GT(11)'
		ty$(11 ,1)='EM(4),'    	: ty$(11 ,2)='pos 118,N 2,'    	: ty$(11 ,3)=',pic(zz)'            	: ty$(11 ,4)= 'DT(12)'	: ty$(11 ,5)= 'GT(12)'
		ty$(12 ,1)='EM(5),'    	: ty$(12 ,2)='pos 120,N 2,'    	: ty$(12 ,3)=',pic(zz)'            	: ty$(12 ,4)= 'DT(13)'	: ty$(12 ,5)= 'GT(13)'
		ty$(13 ,1)='EM(6),'    	: ty$(13 ,2)='pos 122,N 2,'    	: ty$(13 ,3)=',pic(zz)'            	: ty$(13 ,4)= 'DT(14)'	: ty$(13 ,5)= 'GT(14)'
		ty$(14 ,1)='EM(7),'    	: ty$(14 ,2)='pos 124,N 2,'    	: ty$(14 ,3)=',pic(zz)'            	: ty$(14 ,4)= 'DT(15)'	: ty$(14 ,5)= 'GT(15)'
		ty$(15 ,1)='EM(8),'    	: ty$(15 ,2)='pos 126,PD 3.3,'	: ty$(15 ,3)=',pic(---.###)'       	: ty$(15 ,4)= 'DT(16)'	: ty$(15 ,5)= 'GT(16)'
		ty$(16 ,1)='EM(9),'    	: ty$(16 ,2)='pos 129,PD 3.3,'	: ty$(16 ,3)=',pic(---.###)'       	: ty$(16 ,4)= 'DT(17)'	: ty$(16 ,5)= 'GT(17)'
		ty$(17 ,1)='EM(10),'   	: ty$(17 ,2)='pos 132,PD 4.2,'	: ty$(17 ,3)=',pic(--,---.##)'    	: ty$(17 ,4)= 'DT(18)'	: ty$(17 ,5)= 'GT(18)'
		ty$(18 ,1)='EM(11),'   	: ty$(18 ,2)='pos 136,PD 4.2,'	: ty$(18 ,3)=',pic(--,---.##)'    	: ty$(18 ,4)= 'DT(19)'	: ty$(18 ,5)= 'GT(19)'
		ty$(19 ,1)='EM(12),'   	: ty$(19 ,2)='pos 140,PD 4.2,'	: ty$(19 ,3)=',pic(--,---.##)'    	: ty$(19 ,4)= 'DT(20)'	: ty$(19 ,5)= 'GT(20)'
		ty$(20 ,1)='EM(13),'   	: ty$(20 ,2)='pos 144,PD 4.2,'	: ty$(20 ,3)=',pic(--,---.##)'    	: ty$(20 ,4)= 'DT(21)'	: ty$(20 ,5)= 'GT(21)'
		ty$(21 ,1)='EM(14),'   	: ty$(21 ,2)='pos 148,PD 4.2,'	: ty$(21 ,3)=',PIC(--,---.##)'    	: ty$(21 ,4)= 'DT(22)'	: ty$(21 ,5)= 'GT(22)'
		ty$(22 ,1)='EM(15),'   	: ty$(22 ,2)='pos 152,PD 4.2,'	: ty$(22 ,3)=',PIC(--,---.##)'    	: ty$(22 ,4)= 'DT(23)'	: ty$(22 ,5)= 'GT(23)'
		ty$(23 ,1)='EM(16),'   	: ty$(23 ,2)='pos 156,N 6,'    	: ty$(23 ,3)=',pic(zz/zz/zz)'      	: ty$(23 ,4)= 'DT(24)'	: ty$(23 ,5)= 'GT(24)'
		ty$(24 ,1)='LPD,'      	: ty$(24 ,2)='pos 162,N 6,'    	: ty$(24 ,3)=',pic(zz/zz/zz)'      	: ty$(24 ,4)= 'DT(25)'	: ty$(24 ,5)= 'GT(25)'
		ty$(25 ,1)='PH$,'      	: ty$(25 ,2)='pos 168,C 12,'   	: ty$(25 ,3)=',C 12'               	: ty$(25 ,4)= 'DT(26)'	: ty$(25 ,5)= 'GT(26)'
		ty$(26 ,1)='BD,'        	: ty$(26 ,2)='pos 180,N 6,'    	: ty$(26 ,3)=',pic(zz/zz/zz)'      	: ty$(26 ,4)= 'dt(27)'	: ty$(26 ,5)= 'GT(27)'
		ty$(27 ,1)='TDN,'      	: ty$(27 ,2)='pos 9,N 3,'      	: ty$(27 ,3)=',n 3'                	: ty$(27 ,4)= 'DT(28)'	: ty$(27 ,5)= 'GT(28)'
		ty$(28 ,1)='TGL2,'     	: ty$(28 ,2)='pos 15,n 6,'     	: ty$(28 ,3)=',n 6'                	: ty$(28 ,4)= 'DT(29)'	: ty$(28 ,5)= 'GT(29)'
		ty$(29 ,1)='TDT(1),'   	: ty$(29 ,2)='pos 24,N 6,'     	: ty$(29 ,3)=',pic(zz/zz/zz)'      	: ty$(29 ,4)= 'DT(30)'	: ty$(29 ,5)= 'GT(30)'
		ty$(30 ,1)='TDT(2),'   	: ty$(30 ,2)='pos 30,N 6,'     	: ty$(30 ,3)=',pic(zz/zz/zz)'      	: ty$(30 ,4)= 'DT(31)'	: ty$(30 ,5)= 'GT(31)'
		ty$(31 ,1)='TDT(3),'   	: ty$(31 ,2)='pos 36,N 6,'     	: ty$(31 ,3)=',pic(zz/zz/zz)'      	: ty$(31 ,4)= 'DT(32)'	: ty$(31 ,5)= 'GT(32)'
		ty$(32 ,1)='TDT(4),'   	: ty$(32 ,2)='pos 42,N 6,'     	: ty$(32 ,3)=',pic(zz/zz/zz)'      	: ty$(32 ,4)= 'DT(33)'	: ty$(32 ,5)= 'GT(33)'
		ty$(33 ,1)='TCD(1),'   	: ty$(33 ,2)='pos 48,N 2,'     	: ty$(33 ,3)=',pic(zz)'            	: ty$(33 ,4)= 'DT(34)'	: ty$(33 ,5)= 'GT(34)'
		ty$(34 ,1)='TCD(2),'   	: ty$(34 ,2)='pos 50,N 2,'     	: ty$(34 ,3)=',pic(zz)'            	: ty$(34 ,4)= 'DT(35)'	: ty$(34 ,5)= 'GT(35)'
		ty$(35 ,1)='TCD(3),'   	: ty$(35 ,2)='pos 52,N 2,'     	: ty$(35 ,3)=',pic(zz)'            	: ty$(35 ,4)= 'DT(36)'	: ty$(35 ,5)= 'GT(36)'
		ty$(36 ,1)='TLI,'      	: ty$(36 ,2)='pos 54,PD 4.2,'  	: ty$(36 ,3)=',pic(---,---.##)'   	: ty$(36 ,4)= 'DT(37)'	: ty$(36 ,5)= 'GT(37)'
		ty$(37 ,1)='TDET(1),'  	: ty$(37 ,2)='pos 58,PD 4.2,'  	: ty$(37 ,3)=',pic(zz,zzz.##)'    	: ty$(37 ,4)= 'DT(38)'	: ty$(37 ,5)= 'GT(38)'
		ty$(38 ,1)='TDET(2),'  	: ty$(38 ,2)='pos 62,PD 4.2,'  	: ty$(38 ,3)=',pic(zz,zzz.##)'    	: ty$(38 ,4)= 'DT(39)'	: ty$(38 ,5)= 'GT(39)'
		ty$(39 ,1)='TDET(3),'  	: ty$(39 ,2)='pos 66,PD 4.2,'  	: ty$(39 ,3)=',pic(zz,zzz.##)'    	: ty$(39 ,4)= 'DT(40)'	: ty$(39 ,5)= 'GT(40)'
		ty$(40 ,1)='TDET(4),'  	: ty$(40 ,2)='pos 70,PD 4.2,'  	: ty$(40 ,3)=',pic(---,---.##)'   	: ty$(40 ,4)= 'DT(41)'	: ty$(40 ,5)= 'GT(41)'
		ty$(41 ,1)='TDET(5),'  	: ty$(41 ,2)='pos 74,PD 4.2,'  	: ty$(41 ,3)=',pic(---,---.##)'   	: ty$(41 ,4)= 'DT(42)'	: ty$(41 ,5)= 'GT(42)'
		ty$(42 ,1)='TDET(6),'  	: ty$(42 ,2)='pos 78,PD 4.2,'  	: ty$(42 ,3)=',pic(---,---.##)'   	: ty$(42 ,4)= 'DT(43)'	: ty$(42 ,5)= 'GT(43)'
		ty$(43 ,1)='TDET(7),'  	: ty$(43 ,2)='pos 82,PD 4.2,'  	: ty$(43 ,3)=',pic(---,---.##)'   	: ty$(43 ,4)= 'DT(44)'	: ty$(43 ,5)= 'GT(44)'
		ty$(44 ,1)='TDET(8),'  	: ty$(44 ,2)='pos 86,PD 4.2,'  	: ty$(44 ,3)=',pic(---,---.##)'   	: ty$(44 ,4)= 'DT(45)'	: ty$(44 ,5)= 'GT(45)'
		ty$(45 ,1)='TDET(9),'  	: ty$(45 ,2)='pos 90,PD 4.2,'  	: ty$(45 ,3)=',pic(---,---.##)'   	: ty$(45 ,4)= 'DT(46)'	: ty$(45 ,5)= 'GT(46)'
		ty$(46 ,1)='TDET(10),' 	: ty$(46 ,2)='pos 94,PD 4.2,'  	: ty$(46 ,3)=',pic(---,---.##)'   	: ty$(46 ,4)= 'DT(47)'	: ty$(46 ,5)= 'GT(47)'
		ty$(47 ,1)='TDET(11),' 	: ty$(47 ,2)='pos 98,PD 4.2,'  	: ty$(47 ,3)=',pic(---,---.##)'   	: ty$(47 ,4)= 'DT(48)'	: ty$(47 ,5)= 'GT(48)'
		ty$(48 ,1)='TDET(12),' 	: ty$(48 ,2)='pos 102,PD 4.2,'	: ty$(48 ,3)=',pic(---,---.##)'   	: ty$(48 ,4)= 'DT(49)'	: ty$(48 ,5)= 'GT(49)'
		ty$(49 ,1)='TDET(13),' 	: ty$(49 ,2)='pos 106,PD 4.2,'	: ty$(49 ,3)=',pic(---,---.##)'   	: ty$(49 ,4)= 'DT(50)'	: ty$(49 ,5)= 'GT(50)'
		ty$(50 ,1)='TDET(14),' 	: ty$(50 ,2)='pos 110,PD 4.2,'	: ty$(50 ,3)=',pic(---,---.##)'   	: ty$(50 ,4)= 'DT(51)'	: ty$(50 ,5)= 'GT(51)'
		ty$(51 ,1)='TDET(15),' 	: ty$(51 ,2)='pos 114,PD 4.2,'	: ty$(51 ,3)=',pic(---,---.##)'   	: ty$(51 ,4)= 'DT(52)'	: ty$(51 ,5)= 'GT(52)'
		ty$(52 ,1)='TDET(16),' 	: ty$(52 ,2)='pos 118,PD 4.2,'	: ty$(52 ,3)=',pic(---,---.##)'   	: ty$(52 ,4)= 'DT(53)'	: ty$(52 ,5)= 'GT(53)'
		ty$(53 ,1)='TDET(17),' 	: ty$(53 ,2)='pos 122,PD 4.2,'	: ty$(53 ,3)=',pic(---,---.##)'   	: ty$(53 ,4)= 'DT(54)'	: ty$(53 ,5)= 'GT(54)'
		ty$(54 ,1)='TDET(18),' 	: ty$(54 ,2)='pos 126,PD 4.2,'	: ty$(54 ,3)=',pic(---,---.##)'   	: ty$(54 ,4)= 'DT(55)'	: ty$(54 ,5)= 'GT(55)'
		ty$(55 ,1)='TDET(19),' 	: ty$(55 ,2)='pos 130,PD 4.2,'	: ty$(55 ,3)=',pic(---,---.##)'   	: ty$(55 ,4)= 'DT(56)'	: ty$(55 ,5)= 'GT(56)'
		ty$(56 ,1)='TDET(20),' 	: ty$(56 ,2)='pos 134,PD 4.2,'	: ty$(56 ,3)=',pic(---,---.##)'   	: ty$(56 ,4)= 'DT(57)'	: ty$(56 ,5)= 'GT(57)'
		ty$(57 ,1)='TDET(21),' 	: ty$(57 ,2)='pos 138,PD 4.2,'	: ty$(57 ,3)=',pic(---,---.##)'   	: ty$(57 ,4)= 'DT(58)'	: ty$(57 ,5)= 'GT(58)'
		ty$(58 ,1)='TDET(22),' 	: ty$(58 ,2)='pos 142,PD 4.2,'	: ty$(58 ,3)=',pic(---,---.##)'   	: ty$(58 ,4)= 'DT(59)'	: ty$(58 ,5)= 'GT(59)'
		ty$(59 ,1)='TDET(23),' 	: ty$(59 ,2)='pos 146,PD 4.2,'	: ty$(59 ,3)=',pic(---,---.##)'   	: ty$(59 ,4)= 'DT(60)'	: ty$(59 ,5)= 'GT(60)'
		ty$(60 ,1)='tdn,'      	: ty$(60 ,2)='pos 9,n 3,'      	: ty$(60 ,3)=',pic(-###)'          	: ty$(60 ,4)= 'DT(61)'	: ty$(60 ,5)= 'GT(61)'
		ty$(61 ,1)='prd,'      	: ty$(61 ,2)='pos 12,PD 6,'    	: ty$(61 ,3)=',pic(ZZZZ/zz/zz)'   	: ty$(61 ,4)= 'DT(62)'	: ty$(61 ,5)= 'GT(62)'
		ty$(62 ,1)='ckno,'     	: ty$(62 ,2)='pos 18,n 7,'     	: ty$(62 ,3)=',pic(ZZZZzzz)'       	: ty$(62 ,4)= 'DT(63)'	: ty$(62 ,5)= 'GT(63)'
		ty$(63 ,1)='TDc(1),'   	: ty$(63 ,2)='pos 25,PD 3.2,'  	: ty$(63 ,3)=',pic(---,---.##)'   	: ty$(63 ,4)= 'DT(64)'	: ty$(63 ,5)= 'GT(64)'
		ty$(64 ,1)='TDc(2),'   	: ty$(64 ,2)='pos 28,PD 3.2,'  	: ty$(64 ,3)=',pic(---,---.##)'   	: ty$(64 ,4)= 'DT(65)'	: ty$(64 ,5)= 'GT(65)'
		ty$(65 ,1)='TDc(3),'   	: ty$(65 ,2)='pos 31,PD 3.2,'  	: ty$(65 ,3)=',pic(---,---.##)'   	: ty$(65 ,4)= 'DT(66)'	: ty$(65 ,5)= 'GT(66)'
		ty$(66 ,1)='TDc(4),'   	: ty$(66 ,2)='pos 34,PD 3.2,'  	: ty$(66 ,3)=',pic(---,---.##)'   	: ty$(66 ,4)= 'DT(67)'	: ty$(66 ,5)= 'GT(67)'
		ty$(67 ,1)='TDc(5),'   	: ty$(67 ,2)='pos 37,PD 3.2,'  	: ty$(67 ,3)=',pic(---,---.##)'   	: ty$(67 ,4)= 'DT(68)'	: ty$(67 ,5)= 'GT(68)'
		ty$(68 ,1)='TDC(6),'   	: ty$(68 ,2)='pos 40,PD 3.2,'  	: ty$(68 ,3)=',pic(-,---.##)'      	: ty$(68 ,4)= 'DT(69)'	: ty$(68 ,5)= 'GT(69)'
		ty$(69 ,1)='TDC(7),'   	: ty$(69 ,2)='pos 45,PD 5.2,'  	: ty$(69 ,3)=',pic(----,---.##)'  	: ty$(69 ,4)= 'DT(70)'	: ty$(69 ,5)= 'GT(70)'
		ty$(70 ,1)='TDC(8),'   	: ty$(70 ,2)='pos 50,PD 5.2,'  	: ty$(70 ,3)=',pic(----,---.##)'  	: ty$(70 ,4)= 'DT(71)'	: ty$(70 ,5)= 'GT(71)'
		ty$(71 ,1)='TDC(9),'   	: ty$(71 ,2)='pos 55,PD 5.2,'  	: ty$(71 ,3)=',pic(----,---.##)'  	: ty$(71 ,4)= 'DT(72)'	: ty$(71 ,5)= 'GT(72)'
		ty$(72 ,1)='TDC(10),'  	: ty$(72 ,2)='pos 60,PD 5.2,'  	: ty$(72 ,3)=',pic(----,---.##)'  	: ty$(72 ,4)= 'DT(73)'	: ty$(72 ,5)= 'GT(73)'
		ty$(73 ,1)='Tcp(1),'   	: ty$(73 ,2)='pos 65,PD 5.2,'  	: ty$(73 ,3)=',pic(--,---,---.##)'	: ty$(73 ,4)= 'DT(74)'	: ty$(73 ,5)= 'GT(74)'
		ty$(74 ,1)='Tcp(2),'   	: ty$(74 ,2)='pos 70,PD 5.2,'  	: ty$(74 ,3)=',pic(--,---,---.##)'	: ty$(74 ,4)= 'DT(75)'	: ty$(74 ,5)= 'GT(75)'
		ty$(75 ,1)='Tcp(3),'   	: ty$(75 ,2)='pos 75,PD 5.2,'  	: ty$(75 ,3)=',pic(--,---,---.##)'	: ty$(75 ,4)= 'DT(76)'	: ty$(75 ,5)= 'GT(76)'
		ty$(76 ,1)='Tcp(4),'   	: ty$(76 ,2)='pos 80,PD 5.2,'  	: ty$(76 ,3)=',pic(--,---,---.##)'	: ty$(76 ,4)= 'DT(77)'	: ty$(76 ,5)= 'GT(77)'
		ty$(77 ,1)='Tcp(5),'   	: ty$(77 ,2)='pos 85,PD 5.2,'  	: ty$(77 ,3)=',pic(--,---,---.##)'	: ty$(77 ,4)= 'DT(78)'	: ty$(77 ,5)= 'GT(78)'
		ty$(78 ,1)='Tcp(6),'   	: ty$(78 ,2)='pos 90,PD 5.2,'  	: ty$(78 ,3)=',pic(--,---,---.##)'	: ty$(78 ,4)= 'DT(79)'	: ty$(78 ,5)= 'GT(79)'
		ty$(79 ,1)='Tcp(7),'   	: ty$(79 ,2)='pos 95,PD 5.2,'  	: ty$(79 ,3)=',pic(--,---,---.##)'	: ty$(79 ,4)= 'DT(80)'	: ty$(79 ,5)= 'GT(80)'
		ty$(80 ,1)='Tcp(8),'   	: ty$(80 ,2)='pos 100,PD 5.2,'	: ty$(80 ,3)=',pic(--,---,---.##)'	: ty$(80 ,4)= 'DT(81)'	: ty$(80 ,5)= 'GT(81)'
		ty$(81 ,1)='tcp(9),'   	: ty$(81 ,2)='pos 105,PD 5.2,'	: ty$(81 ,3)=',pic(--,---,---.##)'	: ty$(81 ,4)= 'DT(82)'	: ty$(81 ,5)= 'GT(82)'
		ty$(82 ,1)='tcp(10),'  	: ty$(82 ,2)='pos 110,PD 5.2,'	: ty$(82 ,3)=',pic(--,---,---.##)'	: ty$(82 ,4)= 'DT(83)'	: ty$(82 ,5)= 'GT(83)'
		ty$(83 ,1)='tcp(11),'  	: ty$(83 ,2)='pos 115,PD 5.2,'	: ty$(83 ,3)=',pic(--,---,---.##)'	: ty$(83 ,4)= 'DT(84)'	: ty$(83 ,5)= 'GT(84)'
		ty$(84 ,1)='tcp(12),'  	: ty$(84 ,2)='pos 120,PD 5.2,'	: ty$(84 ,3)=',pic(--,---,---.##)'	: ty$(84 ,4)= 'DT(85)'	: ty$(84 ,5)= 'GT(85)'
		ty$(85 ,1)='tcp(13),'  	: ty$(85 ,2)='pos 125,PD 5.2,'	: ty$(85 ,3)=',pic(--,---,---.##)'	: ty$(85 ,4)= 'DT(86)'	: ty$(85 ,5)= 'GT(86)'
		ty$(86 ,1)='tcp(14),'  	: ty$(86 ,2)='pos 130,PD 5.2,'	: ty$(86 ,3)=',pic(--,---,---.##)'	: ty$(86 ,4)= 'DT(87)'	: ty$(86 ,5)= 'GT(87)'
		ty$(87 ,1)='tcp(15),'  	: ty$(87 ,2)='pos 135,PD 5.2,'	: ty$(87 ,3)=',pic(--,---,---.##)'	: ty$(87 ,4)= 'DT(88)'	: ty$(87 ,5)= 'GT(88)'
		ty$(88 ,1)='tcp(16),'  	: ty$(88 ,2)='pos 140,PD 5.2,'	: ty$(88 ,3)=',pic(--,---,---.##)'	: ty$(88 ,4)= 'DT(89)'	: ty$(88 ,5)= 'GT(89)'
		ty$(89 ,1)='tcp(17),'  	: ty$(89 ,2)='pos 145,PD 5.2,'	: ty$(89 ,3)=',pic(--,---,---.##)'	: ty$(89 ,4)= 'DT(90)'	: ty$(89 ,5)= 'GT(90)'
		ty$(90 ,1)='tcp(18),'  	: ty$(90 ,2)='pos 150,PD 5.2,'	: ty$(90 ,3)=',pic(--,---,---.##)'	: ty$(90 ,4)= 'DT(91)'	: ty$(90 ,5)= 'GT(91)'
		ty$(91 ,1)='tcp(19),'  	: ty$(91 ,2)='pos 155,PD 5.2,'	: ty$(91 ,3)=',pic(--,---,---.##)'	: ty$(91 ,4)= 'DT(92)'	: ty$(91 ,5)= 'GT(92)'
		ty$(92 ,1)='tcp(20),'  	: ty$(92 ,2)='pos 160,PD 5.2,'	: ty$(92 ,3)=',pic(--,---,---.##)'	: ty$(92 ,4)= 'DT(93)'	: ty$(92 ,5)= 'GT(93)'
		ty$(93 ,1)='tcp(21),'  	: ty$(93 ,2)='pos 165,PD 5.2,'	: ty$(93 ,3)=',pic(--,---,---.##)'	: ty$(93 ,4)= 'DT(94)'	: ty$(93 ,5)= 'GT(94)'
		ty$(94 ,1)='Tcp(22),'  	: ty$(94 ,2)='pos 170,PD 5.2,'	: ty$(94 ,3)=',pic(--,---,---.##)'	: ty$(94 ,4)= 'DT(95)'	: ty$(94 ,5)= 'GT(95)'
		ty$(95 ,1)='Tcp(23),'  	: ty$(95 ,2)='pos 175,PD 5.2,'	: ty$(95 ,3)=',pic(--,---,---.##)'	: ty$(95 ,4)= 'DT(96)'	: ty$(95 ,5)= 'GT(96)'
		ty$(96 ,1)='tcp(24),'  	: ty$(96 ,2)='pos 175,PD 5.2,'	: ty$(96 ,3)=',pic(--,---,---.##)'	: ty$(96 ,4)= 'DT(97)'	: ty$(96 ,5)= 'GT(97)'
		ty$(97 ,1)='Tcp(25),'  	: ty$(97 ,2)='pos 180,PD 5.2,'	: ty$(97 ,3)=',pic(--,---,---.##)'	: ty$(97 ,4)= 'DT(98)'	: ty$(97 ,5)= 'GT(98)'
		ty$(98 ,1)='tcp(26),'  	: ty$(98 ,2)='pos 185,PD 5.2,'	: ty$(98 ,3)=',pic(--,---,---.##)'	: ty$(98 ,4)= 'DT(99)'	: ty$(98 ,5)= 'GT(99)'
		ty$(99 ,1)='Tcp(27),'  	: ty$(99 ,2)='pos 190,PD 5.2,'	: ty$(99 ,3)=',pic(--,---,---.##)'	: ty$(99 ,4)='DT(100)'	: ty$(99 ,5)='GT(100)'
		ty$(100,1)='tcp(28),'  	: ty$(100,2)='pos 195,PD 5.2,'	: ty$(100,3)=',pic(--,---,---.##)'	: ty$(100,4)='DT(101)'	: ty$(100,5)='GT(101)'
		ty$(101,1)='Tcp(29),'  	: ty$(101,2)='pos 200,PD 5.2,'	: ty$(101,3)=',pic(--,---,---.##)'	: ty$(101,4)='DT(102)'	: ty$(101,5)='GT(102)'
		ty$(102,1)='tcp(30),'  	: ty$(102,2)='pos 205,PD 5.2,'	: ty$(102,3)=',pic(--,---,---.##)'	: ty$(102,4)='DT(103)'	: ty$(102,5)='GT(103)'
		ty$(103,1)='Tcp(31),'  	: ty$(103,2)='pos 210,PD 5.2,'	: ty$(103,3)=',pic(--,---,---.##)'	: ty$(103,4)='DT(104)'	: ty$(103,5)='GT(104)'
		ty$(104,1)='tcp(32),'  	: ty$(104,2)='pos 215,PD 5.2,'	: ty$(104,3)=',pic(--,---,---.##)'	: ty$(104,4)='DT(105)'	: ty$(104,5)='GT(105)'
		dim ty$(104,5)*20
	! /r
fnend
include: ertn