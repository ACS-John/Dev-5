! Replace S:\Core\Label\fnAddLabel
! add a label to a queue that will be printed when fnLabel is called
def library fnaddlabel(mat in_labeltext$)
	library 'S:\Core\Library': fngethandle
	on error goto ERTN
	dim labeltext$(5)*120
	
	if udim(in_labeltext$)<>5 then 
		pr "fnAddLabel - You should send no more than 5 array items to fnAddLabel."
		pause
	end if
	for j=1 to min(5,udim(in_labeltext$)) 
		labeltext$(j)=in_labeltext$(j)(1:min(len(in_labeltext$(j)),120)) 
	next j
	open #tmp:=fngethandle: "Name="&env$('temp')&"\Label.dat,RecL=600,Use",internal,output  
	write #tmp,using "Form POS 1,5*C 120": mat labeltext$ 
	close #tmp: 
	mat labeltext$=("")
	goto XIT
	XIT: !
fnend 

include: ertn