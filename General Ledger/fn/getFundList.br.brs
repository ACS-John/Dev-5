def library fnGetFundList(mat fund_list)
  ! returns an array of all unique gl number funds
  autoLibrary
  open #company=fnH: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input 
  read #company,using 'Form Pos 150,N 1': use_dept
  close #company: 
  if use_dept then 
	 mat fund_list(999)
	 open #gfl_h_glmstr:=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed 
	 do 
		read #gfl_h_glmstr,using 'form pos 1,N 3': fund eof GFL_EO_GLMSTR
		if fund<>fund_prior then 
		  fund_list_count+=1
		  fund_list(fund_list_count)=fund
		  fund_prior=fund
		end if 
	 loop 
	 GFL_EO_GLMSTR: ! 
	 close #gfl_h_glmstr: 
  else ! no departments
	 fund_list_count=0
  end if 
  mat fund_list(fund_list_count)
fnend 