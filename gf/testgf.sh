echo "p \"${1}\" | vp | wf -file=test.tmp" | gf -run BigTestSwe.gf
dot -Tpdf test.tmp > test.pdf 
xpdf test.pdf &
 
