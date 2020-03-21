echo "-------------------------"
echo "-> ELEVATOR BASH COMPILER"
echo "-------------------------"
erlc *.erl;
echo "-------------------------"
echo "-> BOOTING"
echo "-------------------------"
erl -s main start


