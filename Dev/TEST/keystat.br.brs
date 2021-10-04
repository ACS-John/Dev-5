! Replace S:\Core\KeyStat
	on error goto Ertn
	exec 'con gui off'
	close #101: ioerr ignore
	open #101: "SRow=2,SCol=5,ERow=22,ECol=37,Border=ss,Caption=KeyStat",display,outIn 
	pr #101: newpage
	pr #101: "Press any key to see it's keystat"
	pr #101: "      unhex and hex values."
	pr #101: "    Or press CTRL+A for ATTN"
	pr #101: "      ------------------------"
	pr #101: " KeyStat is a workstation Basic"
	pr #101: "            function."
	pr #101: "      ------------------------"
	for j=1 to 13
		pr #101: "                                 "
	next j
do
	k$=kstat$
	if k$<>"" then 
		pr #101: " UnHex: "&unhex$(k$)
		pr #101: "   Hex: "&k$
		pr #101: "  FKey: "&str$(fkey)
		pr #101: " "
	end if
loop
include: ertn
