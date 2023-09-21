pr program$&' library is not intended to be run directly.'
end


def library fnOpenFsdAcglfnsIJ(&mp1,&mp2; ___,returnN)
	autoLibrary
	if fnPs=2 then
		mp1=72
		mp2=78
		open #returnN=fnH: 'Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr',i,i,k
	else
		mp1=69
		mp2=75
		open #returnN=fnH: 'Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr',i,i,k
	end if
	fnOpenFsdAcglfnsIJ=returnN
fnend

def library fnOpenFsDesignInput(type$; forceHandle, ___,returnN,fl1$*256)
	autoLibrary
	if lwrc$(type$)='balance sheet' then
		if fnps=2 then
			mp1=66
			fl1$='Name=[Q]\GLmstr\AcGLFnSc.h[cno],KFName=[Q]\GLmstr\agfsidx1.h[cno],Shr'
		else
			mp1=63
			fl1$='Name=[Q]\GLmstr\ACGLFNSB.h[cno],KFName=[Q]\GLmstr\agfsidx4.h[cno],Shr'
		end if
	else if lwrc$(type$)='' then
		pr 'blank type in fnOpenFsDesignInput'
		pause
	else
		pr 'unrecognized type ('&type$&') in fnOpenFsDesignInput'
		pause
	end if
	if forceHandle then returnN=forceHandle else returnN=fnH
	open #returnN: fl1$,i,i,k ! formerly #1
	fnOpenFsDesignInput=returnN
fnend




def library fnFsIndexFundStmt
	autoLibrary
	fnFsIndexFundStmt=fn_fsIndex(75,78)
fnend
def library fnFsIndexIncStmt
	autoLibrary
	fnFsIndexIncStmt=fn_fsIndex(69,72)
fnend
def library fnFsIndexBalSht
	autoLibrary
	fnFsIndexBalSht=fn_fsIndex(63,66)
fnend
	def fn_fsIndex(mp1,mp2; ___,returnN)
		autoLibrary
		if fnPs=2 then ! Secondary
			exe 'con sub [FinancialStatementCode] C'
			returnN=fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[temp]\fsindex.h[cno]',str$(mp2)&' 3')
		else ! Primary
			exe 'con sub [FinancialStatementCode] B'
			returnN=fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[temp]\fsindex.h[cno]',str$(mp1)&' 3')
		end if
		fn_fsIndex=returnN
	fnend
