! replace S:\Core\barcode.br
BARCODE: !  produces code 39 barcodes for any system
! must set these variables: rightleft  (how far from the right side)                                        updown (how fare down you want the barcode                                      barcode$ (the customer#, amount etc to be coded
! pr ace work file number must be 20
	def library fnbarcode(barcode$,rightleft,updown)
SET_VARIABLES: ! 
		w=rightleft ! 30 ! right or left
		x=updown ! 25 ! up and down position of top of line
		y=w ! width of line
		z=6 ! height of line
		blankline=2.0
		q=p=0
		double=.12
! bARCODE$="123567890" ! 345" !67890" ! =z$  kj ! 1,2,3,4,5,6,7,8,9,0 ok
		pr #20: 'Call Print.MyFontBold(1)'
		gosub QUIET
		for a=1 to 10
			barcode=val(barcode$(a:a))
			p=pos(barcode$,".",a) : if p=a then goto PERIOD ! searching for period
			q=pos(barcode$,"0",a) : if q=a then goto ZERO ! searching for zero
			on barcode goto ONE ,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE,ZERO none L210
L210: next a
		gosub QUIET
		pr #20: 'Call Print.MyFontBold(0)'
		goto Xit
ONE: ! 
		w+=double*2 ! blank space
! first line, first character (wide line)
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
		next j
! second line of number 1 (narrow line)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
! third line of character 1 (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line of number 1 (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! fifth line of number one (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
TWO: ! 
		w+=double*2 ! blank space
! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
! second line  (wide line)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! fifth line (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
THREE: ! 
		w+=double*2 ! blank space
! first line (wide line)
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide
		next j
! second line  (wide line)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
! 
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
FOUR: ! 
		w+=double*2 ! blank space
! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
! second line  (narrow line)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
! 
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! fifth line (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
FIVE: ! 
		w+=double*2 ! blank space
! first line (wide line)
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! second line  (narrow line)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
SIX: ! 
		w+=double*2 ! blank space
! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
! second line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
! 
		next j
! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
SEVEN: ! 
		w+=double*2 ! blank space
! first line (narrow line)
! 
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (wide)
! 
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
! 
		next j
! fifth line (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
EIGHT: ! 
		w+=double*2 ! blank space
! first line (wide line)
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
! 
		next j
! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
NINE: ! 
		w+=double*2 ! blank space
! first line (narrow line)
! 
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
! second line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
! 
		next j
		w+=double*blankline ! extra blank line
! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
! 
		next j
! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
ZERO: ! 
		w+=double*2 ! blank space
! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
! 
		next j
! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
QUIET: ! 
		w+=double*2 ! blank line
! first line, quiet zone (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
		next j
		w+=double*blankline ! double blank line
! second line, quiet zone (narrow line)
		w+=double*2 !  blank line
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
		next j
! third  line, quiet zone (wide line)
		w+=double*2 ! blank line
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
		next j
! 4th line, quiet zone (wide line)
		w+=double*2 ! blank line
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide   line
		next j
! 5th line, quiet zone (narrow line)
		w+=double*2 ! blank line
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
		next j
		return 
PERIOD: ! 
		w+=double*2 ! blank space
! first line (big line)
! 
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		w+=double*blankline ! extra blank line
! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto L210
Xit: fnend 
