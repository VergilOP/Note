#!/bin/bash

#globals
ret=0
d=/dev/opsysmem
executable=charDeviceDriver

# --- helper function ---
function run(){
	echo -e "$1:"
	$1 #execute
	#check errors
	tmp=$?
	if [ $tmp -ne 0 ]; then
		ret=$tmp
	fi
	echo "" #newline
	return $tmp
}


# --- TESTCASES ---
function basic_testcase(){
	t="testcase 1"

	#cleanup
	rmmod $executable 2>/dev/null >/dev/null
	rm -f $d

	#load kernel driver
	echo -en " load kernel driver:\t"
	insmod ./$executable.ko
	if [ $? -ne 0 ]
	then
		echo -e "ERROR: insmod failed"
		return -1
	else
		echo "ok"
	fi



	if [ ! -c $d ];
	then

		#mknod
		echo -en " mknod:\t\t\t"
		major=`dmesg | tail -n 6 | sed -n "s/\[[0-9. ]*\] 'mknod \/dev\/opsysmem c \([0-9]*\) 0'\./\1/p"`
		mknod $d c $major 0
		if [ $? -ne 0 ]
		then
			echo -e "ERROR: mknod command failed"
			rmmod $executable
			return 1
		else
			echo "ok"
		fi
	fi

	#check file
	echo -en " ls $d:\t"
	line=`ls $d 2>/dev/null`
	if [ "$line" != "$d" ]
	then
		echo -e "ERROR: file $d does not exist after loading the kernel module"
		rmmod $executable
		return -1
	else
		echo "ok"
	fi

	#write test
	echo -en " write test:\t\t"
	echo "$t" > $d 2>/dev/null
	if [ $? -ne 0 ]
	then
		echo -e "ERROR: writing $d failed"
		rmmod $executable
		return -1
	else
		echo "ok"
	fi

	#read test
	echo -en " read test:\t\t"
	r=`head -n 1 < $d`
	if [ $? -ne 0 ]
	then
		echo -e "ERROR: reading $d failed"
		rmmod $executable
		return -1
	fi

	#check if same was read
	if [ "$r" != "$t" ]
	then
		echo -e "ERROR: $d: could not read what was written before"
		rmmod $executable
		return -1
	else
		echo "ok"
	fi

	#unload module
	echo -en " unload module:\t\t"
	rmmod $executable
	if [ $? -ne 0 ]
	then
		echo -e "ERROR: unloading kernel module failed"
		return -1
	else
		echo "ok"
	fi

	return 0
}

# --- execution ---
#reset
rmmod $executable 2>/dev/null

run basic_testcase
#cleanup
rm -f $d

exit $ret

