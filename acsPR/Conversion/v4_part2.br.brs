! Replace S:\acsPR\conversion\v4_part2
fn_setup

fnTop(program$,'add missing files and indexes')
! r: do every company
	fngetdir2('[Q]\'&fncursys$&'mstr',mat filename$,'/od /ta','Company.*')
	company_count=filename_item=0
	for filename_item=1 to udim(mat filename$)
		tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
		if tmp_cno<>99999 and filename$(filename_item)<>'' then ! don't display company 99999
			cno=tmp_cno
			fn_pr_conversion_add_missing(cno)
		end if
		ACNO_CONV: !
	next filename_item
! /r
end  !
def fn_setup
	autoLibrary
	on error goto Ertn

	dim a$*40,em$*30,ta(2),cp(32),tcp(22),hc(5),thc(5),d$*20,whc(10)
	dim dedcode(10),calcode(10),dedfed(10),message$*40
	dim tcp(32),newtdc(10),newtdet(23),tdt(4),tcd(3),tdet(17),tdy(6),tdc(6)
	dim ty(21),tqm(17),tcp(22),tdet(17),dednames$(20)*20,d1$*20
fnend
def library fnpr_conversion_add_missing(cno)
		if ~setup then fn_setup
		fnpr_conversion_add_missing=fn_pr_conversion_add_missing(cno)
fnend
def fn_pr_conversion_add_missing(cno)
	fnStatus('PR adding missing files and indexes - Company Number [cno]')
	open #14: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]',i,outIn,k ioerr L2180
	goto L2200
	L2180: !
	open #14: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],RecL=224,kps=1,kln=17,replace',i,outIn,k
	L2200: !
	close #14:
	open #30: 'Name=[Q]\PRmstr\dd.h[cno],RecL=72,KFName=[Q]\PRmstr\DDidx1.h[cno],kps=1,kln=10,Use',i,outIn,k
	close #30:
	fnindex_sys(cno,'PR')
	goto Xit
Xit: !
fnend
include: ertn
