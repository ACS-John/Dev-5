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
def library fnFsIndex(mp1,mp2; ___,returnN)
	autoLibrary
	if fnPs=2 then ! Secondary
		exe 'con sub [FinancialStatementCode] C'
		returnN=fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[Q]\[temp]\fsindex.h[cno]',str$(mp2)&' 3')
	else ! Primary
		exe 'con sub [FinancialStatementCode] B'
		returnN=fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[Q]\[temp]\fsindex.h[cno]',str$(mp1)&' 3')
	end if
	fnFsIndex=returnN
fnend
