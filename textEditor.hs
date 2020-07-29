--b = setText create "my main text in editor"
data Editor = Editor (String, String, Int) deriving (Show)
				--main text,clipboard,pos

--create function init values
create :: Editor
create = Editor("","",0)

-- Editor: main text output as string
getText :: Editor -> String 
getText (Editor(a,_,_)) = a

--Editor: sets maintext to the value of inputted string (x)
setText :: Editor -> String -> Editor
setText (Editor(_,b,c)) x = Editor (x,b,c)

-- Editor: output clipboard as string
getClipboard :: Editor -> String
getClipboard (Editor(_,b,_)) = b

--Editor: sets values of clipboard to inputted string 
setClipboard :: Editor -> String -> Editor
setClipboard (Editor(a,_, c)) x = Editor (a, x, c)

--Editor sets cursor as inputted interger above 0 and less than the length of maintext
setCursorPos :: Editor -> Int -> Editor
setCursorPos (Editor(a, b, c)) x = 
    if x >= 0 && x < length a 
       then Editor (a, b, x)
       else Editor (a, b, c)

--Editor: output cursor pos as int 
getCursorPos :: Editor -> Int
getCursorPos(Editor( _,_,c)) = c

--sets cursor position to 0 (start)
cursorStart :: Editor -> Editor
cursorStart (Editor (a,b,c)) = (Editor (a, b, 0)) 

--sets cursor position to max lenght a (a=maintext)
cursorEnd :: Editor -> Editor
cursorEnd (Editor (a,b,c)) = (Editor (a, b, length a))

--goes right by increasing cursor position, stops at the max length of maintext
right :: Editor -> Editor
right (Editor(a, b, c)) =
	if ((c+1) < length a ) 
       then Editor (a, b,(c+1))
       else Editor (a, b, c)

--goes left by decreasing cursor position, stops at the start of maintext
left :: Editor -> Editor
left (Editor(a, b, c)) =
	if ((c-1) > 0 ) 
       then Editor (a, b,(c-1))
       else Editor (a, b, c)



-- editor: split maintext(a) where cursor(c) and insert inputted string(x)
insert :: Editor -> String -> Editor
insert (Editor(a, b, c)) x = 
    let (a1, a2) = splitAt c a 
    in Editor (a1++x++a2, b, c)

-- editor: split maintext(a) where cursor(c) and insert clipboard(b)
paste :: Editor -> Editor
paste (Editor(a, b, c)) = 
    let (a1, a2) = splitAt c a 
    in Editor (a1++b++a2, b, c)

--Editor: output string of prefix of as(front of maintext) of a list up to cursor postion
leftSelection:: Editor -> String
leftSelection (Editor(a,b,c))  =
	if c > 0 
		then take c a  
		else ""

--Editor: output string of sufix of as(end of maintext) of a up to cursor postion	
rightSelection:: Editor -> String
rightSelection (Editor(a,b,c))  =
	if c > 0 
		then drop c a  
		else ""

--Editor: if cursor is inside boundry, maintext is droped up to the cursor location 
delLeft:: Editor -> Editor
delLeft (Editor (a,b,c)) =
	if c >=0
		then Editor(a1,b,c)
		else Editor (a,b,c)
	where
		a1  = drop c a 

--Editor: if cursor is inside boundry, maintext is taken up to the cursor location (deleting)
delRight:: Editor -> Editor
delRight (Editor (a,b,c)) =
	if c <= length a
		then Editor(a1,b,c)
		else Editor (a,b,c)
	where
		a1  = take c a 
    




-- IO FUNCTIONS
-- load file to string based off of file name
loadFile :: String -> IO ()
loadFile x = do  
    contents <- readFile x
    putStr $ contents
    let t = putStr $ contents
    return (t)
    putStr ""

--Editor: output maintext(a) to file called output.txt and then user recives notfication that its done
save:: Editor -> IO ()
save (Editor(a,b,c)) = do
	writeFile "output.txt" a
	print "file saved"

