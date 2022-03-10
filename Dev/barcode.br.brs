autoLibrary

! open #20: 'Name=[Q]\UBmstr\label'&wsid$&'.txt,Replace,RecL=5000',d,o
fnpa_open
! pr #20: 'Call Print.MyOrientation('Portrait')'
fnpa_barcode(1,20,'123456789012')

fnpa_finis

stop
