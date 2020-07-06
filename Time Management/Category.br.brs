fn_setup
fn_setupCategories
fnTop(program$)
fnhamsterfio('TM Category')
Xit: fnXit
def fn_setupCategories
	if ~setupCategories then
		setupCategories=1
		if exists('S:\Core\Data\acsllc\TMCat.h[cno]') then
			dim oldCat$(30)*30
			open #hCategory:=fngethandle: "Name=S:\Core\Data\acsllc\TMCat.h[cno],Shr",internal,input,relative
			read #hCategory,using 'form pos 1,30*c 30',rec=1: mat oldCat$
			dim catData$(0)*80,catDataN(0)
			hCategory=fn_open('TM Category',mat catData$,mat catDataN,mat form$)
			for x=1 to 30
				if trim$(oldCat$(x))<>'' then
					catDataN(cat_id)=x
					catData$(cat_name)=oldCat$(x)
					write #hCategory,using form$(hCategory): mat catData$,mat catDataN
				end if
			nex x
			fnCloseFile(hCategory,'TM Category')
			close #hCategory:
			fnFree('S:\Core\Data\acsllc\TMCat.h[cno]')
		end if
	end if
fnend

def library fnRead30Categories(mat dimTo30$)
	if ~setup then fn_setup
	if ~setupCategories then let fn_setupCategories
	hCategory=fn_open('TM Category',mat catData$,mat catDataN,mat form$)
	mat dimTo30$=('')
	for x=1 to 30
		read #hCategory,key=cnvrt$('N 3',x): mat catData$,mat catDataN nokey R3cNoKey
		dimTo30$(x)=catData$(cat_name)
		R3cNoKey: !
	nex x
	fnCloseFile(hCategory,'TM Category')
fnend
include: fn_open
include: fn_setup
