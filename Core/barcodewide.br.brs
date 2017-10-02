00010 ! replace S:\Core\barcodewide.br
00020 BARCODE: !  produces code 39 barcodes for any system
00030 ! must set these variables: rightleft  (how far from the right side)                                        updown (how fare down you want the barcode                                      barcode$ (the customer#, amount etc to be coded
00040 ! pr ace work file number must be 20
00050   def library fnbarcodewide(barcode$,rightleft,updown)
00060 SET_VARIABLES: ! 
00070     let w=rightleft ! 30 ! right or left
00080     let x=updown ! 25 ! up and down position of top of line
00090     let y=w ! width of line
00100     let z=6 ! height of line
00110     blankline=2.0
00120     let q=p=0
00130     let double=.12
00140 ! bARCODE$="123567890" ! 345" !67890" ! =z$  kj ! 1,2,3,4,5,6,7,8,9,0 ok
00150     gosub QUIET
00160     for a=1 to 10
00170       barcode=val(barcode$(a:a))
00180       let p=pos(barcode$,".",a) : if p=a then goto PERIOD ! searching for period
00190       let q=pos(barcode$,"0",a) : if q=a then goto ZERO ! searching for zero
00200       on barcode goto ONE ,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE,ZERO none L210
00210 L210: next a
00220     gosub QUIET
00230     goto XIT
00240 ONE: ! 
00250     let w+=double*2 ! blank space
00260 ! first line, first character (wide line)
00270     for j=1 to 6
00280       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
00290     next j
00300 ! second line of number 1 (narrow line)
00310     let w+=double*2 ! blank space
00320     for j=1 to 4
00330       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
00340     next j
00350     let w+=double*blankline ! extra blank line
00360 ! third line of character 1 (narrow) (has a blank line in front of it)
00370     let w+=double*2 ! blank space
00380     for j=1 to 4
00390       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
00400     next j
00410 ! fourth line of number 1 (narrow)
00420     let w+=double*2 ! blank space
00430     for j=1 to 4
00440       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
00450     next j
00460 ! fifth line of number one (wide)
00470     let w+=double*2 ! blank space
00480     for j=1 to 6
00490       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
00500     next j
00510     goto L210
00520 TWO: ! 
00530     let w+=double*2 ! blank space
00540 ! first line (narrow line)
00550     for j=1 to 2
00560       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
00570     next j
00580 ! second line  (wide line)
00590     let w+=double*2 ! blank space
00600     for j=1 to 6
00610       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
00620     next j
00630     let w+=double*blankline ! extra blank line
00640 ! third line (narrow) (has a blank line in front of it)
00650     let w+=double*2 ! blank space
00660     for j=1 to 4
00670       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
00680     next j
00690 ! fourth line  (narrow)
00700     let w+=double*2 ! blank space
00710     for j=1 to 4
00720       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
00730     next j
00740 ! fifth line (wide)
00750     let w+=double*2 ! blank space
00760     for j=1 to 6
00770       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
00780     next j
00790     goto L210
00800 THREE: ! 
00810     let w+=double*2 ! blank space
00820 ! first line (wide line)
00830     for j=1 to 6
00840       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide
00850     next j
00860 ! second line  (wide line)
00870     let w+=double*2 ! blank space
00880     for j=1 to 6
00890       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
00900     next j
00910     let w+=double*blankline ! extra blank line
00920 ! third line (narrow) (has a blank line in front of it)
00930     let w+=double*2 ! blank space
00940     for j=1 to 4
00950       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
00960     next j
00970 ! fourth line  (narrow)
00980     let w+=double*2 ! blank space
00990     for j=1 to 4
01000 ! 
01010       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01020     next j
01030 ! fifth line (narrow)
01040     let w+=double*2 ! blank space
01050     for j=1 to 4
01060       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01070     next j
01080     goto L210
01090 FOUR: ! 
01100     let w+=double*2 ! blank space
01110 ! first line (narrow line)
01120     for j=1 to 4
01130       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
01140     next j
01150 ! second line  (narrow line)
01160     let w+=double*2 ! blank space
01170     for j=1 to 4
01180       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01190     next j
01200     let w+=double*blankline ! extra blank line
01210 ! third line (wide) (has a blank line in front of it)
01220     let w+=double*2 ! blank space
01230     for j=1 to 6
01240       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
01250     next j
01260 ! fourth line  (narrow)
01270     let w+=double*2 ! blank space
01280     for j=1 to 4
01290 ! 
01300       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01310     next j
01320 ! fifth line (wide)
01330     let w+=double*2 ! blank space
01340     for j=1 to 6
01350       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01360     next j
01370     goto L210
01380 FIVE: ! 
01390     let w+=double*2 ! blank space
01400 ! first line (wide line)
01410     for j=1 to 6
01420       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01430     next j
01440 ! second line  (narrow line)
01450     let w+=double*2 ! blank space
01460     for j=1 to 4
01470       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01480     next j
01490     let w+=double*blankline ! extra blank line
01500 ! third line (wide) (has a blank line in front of it)
01510     let w+=double*2 ! blank space
01520     for j=1 to 6
01530       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
01540     next j
01550 ! fourth line  (narrow)
01560     let w+=double*2 ! blank space
01570     for j=1 to 4
01580       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01590     next j
01600 ! fifth line (narrow)
01610     let w+=double*2 ! blank space
01620     for j=1 to 4
01630       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01640     next j
01650     goto L210
01660 SIX: ! 
01670     let w+=double*2 ! blank space
01680 ! first line (narrow line)
01690     for j=1 to 4
01700       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
01710     next j
01720 ! second line  (wide)
01730     let w+=double*2 ! blank space
01740     for j=1 to 6
01750       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01760     next j
01770     let w+=double*blankline ! extra blank line
01780 ! third line (wide) (has a blank line in front of it)
01790     let w+=double*2 ! blank space
01800     for j=1 to 6
01810       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
01820     next j
01830 ! fourth line  (narrow)
01840     let w+=double*2 ! blank space
01850     for j=1 to 4
01860       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01870 ! 
01880     next j
01890 ! fifth line (narrow)
01900     let w+=double*2 ! blank space
01910     for j=1 to 4
01920       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
01930     next j
01940     goto L210
01950 SEVEN: ! 
01960     let w+=double*2 ! blank space
01970 ! first line (narrow line)
01980 ! 
01990     for j=1 to 4
02000       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
02010     next j
02020 ! second line  (narrow)
02030     let w+=double*2 ! blank space
02040     for j=1 to 4
02050       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02060     next j
02070     let w+=double*blankline ! extra blank line
02080 ! third line (narrow) (has a blank line in front of it)
02090     let w+=double*2 ! blank space
02100     for j=1 to 4
02110       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
02120     next j
02130 ! fourth line  (wide)
02140 ! 
02150     let w+=double*2 ! blank space
02160     for j=1 to 6
02170       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02180 ! 
02190     next j
02200 ! fifth line (wide)
02210     let w+=double*2 ! blank space
02220     for j=1 to 6
02230       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02240     next j
02250     goto L210
02260 EIGHT: ! 
02270     let w+=double*2 ! blank space
02280 ! first line (wide line)
02290     for j=1 to 6
02300       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02310     next j
02320 ! second line  (narrow)
02330     let w+=double*2 ! blank space
02340     for j=1 to 4
02350       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02360     next j
02370     let w+=double*blankline ! extra blank line
02380 ! third line (narrow) (has a blank line in front of it)
02390     let w+=double*2 ! blank space
02400     for j=1 to 4
02410       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
02420     next j
02430 ! fourth line  (wide)
02440     let w+=double*2 ! blank space
02450     for j=1 to 6
02460       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02470 ! 
02480     next j
02490 ! fifth line (narrow)
02500     let w+=double*2 ! blank space
02510     for j=1 to 4
02520       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02530     next j
02540     goto L210
02550 NINE: ! 
02560     let w+=double*2 ! blank space
02570 ! first line (narrow line)
02580 ! 
02590     for j=1 to 4
02600       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
02610     next j
02620 ! second line  (wide)
02630     let w+=double*2 ! blank space
02640     for j=1 to 6
02650       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02660 ! 
02670     next j
02680     let w+=double*blankline ! extra blank line
02690 ! third line (narrow) (has a blank line in front of it)
02700     let w+=double*2 ! blank space
02710     for j=1 to 4
02720       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
02730     next j
02740 ! fourth line  (wide)
02750     let w+=double*2 ! blank space
02760     for j=1 to 6
02770       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02780 ! 
02790     next j
02800 ! fifth line (narrow)
02810     let w+=double*2 ! blank space
02820     for j=1 to 4
02830       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02840     next j
02850     goto L210
02860 ZERO: ! 
02870     let w+=double*2 ! blank space
02880 ! first line (narrow line)
02890     for j=1 to 4
02900       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
02910     next j
02920 ! second line  (narrow)
02930     let w+=double*2 ! blank space
02940     for j=1 to 4
02950       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
02960     next j
02970     let w+=double*blankline ! extra blank line
02980 ! third line (wide) (has a blank line in front of it)
02990     let w+=double*2 ! blank space
03000     for j=1 to 6
03010       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
03020     next j
03030 ! fourth line  (wide)
03040     let w+=double*2 ! blank space
03050     for j=1 to 6
03060       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
03070 ! 
03080     next j
03090 ! fifth line (narrow)
03100     let w+=double*2 ! blank space
03110     for j=1 to 4
03120       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
03130     next j
03140     goto L210
03150 QUIET: ! 
03160     let w+=double*2 ! blank line
03170 ! first line, quiet zone (narrow line)
03180     for j=1 to 4
03190       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
03200     next j
03210     let w+=double*blankline ! double blank line
03220 ! second line, quiet zone (narrow line)
03230     let w+=double*2 !  blank line
03240     for j=1 to 4
03250       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
03260     next j
03270 ! third  line, quiet zone (wide line)
03280     let w+=double*2 ! blank line
03290     for j=1 to 6
03300       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
03310     next j
03320 ! 4th line, quiet zone (wide line)
03330     let w+=double*2 ! blank line
03340     for j=1 to 6
03350       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide   line
03360     next j
03370 ! 5th line, quiet zone (narrow line)
03380     let w+=double*2 ! blank line
03390     for j=1 to 4
03400       pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
03410     next j
03420     return 
03430 PERIOD: ! 
03440     let w+=double*2 ! blank space
03450 ! first line (big line)
03460 ! 
03470     for j=1 to 6
03480       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
03490     next j
03500     let w+=double*blankline ! extra blank line
03510 ! second line  (narrow)
03520     let w+=double*2 ! blank space
03530     for j=1 to 2
03540       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
03550     next j
03560 ! third line (narrow) (has a blank line in front of it)
03570     let w+=double*2 ! blank space
03580     for j=1 to 2
03590       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
03600     next j
03610 ! fourth line  (wide)
03620     let w+=double*2 ! blank space
03630     for j=1 to 6
03640       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
03650     next j
03660 ! fifth line (narrow)
03670     let w+=double*2 ! blank space
03680     for j=1 to 2
03690       pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
03700     next j
03710     goto L210
03720 XIT: fnend 
