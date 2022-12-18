/*
 *  chardev.c: Creates a read-only char device that says how many times
 *  you've read from the dev file
 */

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/fs.h>
#include <linux/list.h>
#include <linux/slab.h>
#include <asm/uaccess.h>	/* for put_user */
#include <charDeviceDriver.h>
#include "ioctl.h"

MODULE_LICENSE("GPL");

/* 
 * This function is called whenever a process tries to do an ioctl on our
 * device file. We get two extra parameters (additional to the inode and file
 * structures, which all device functions get): the number of the ioctl called
 * and the parameter given to the ioctl function.
 *
 * If the ioctl is write or read/write (meaning output is returned to the
 * calling process), the ioctl call returns the output of this function.
 *
 */

DEFINE_MUTEX  (devLock);
DEFINE_MUTEX  (listLock);
static int counter = 0;

int lengthoflist(void){
	int count = 0;
	struct list_head *pos, *head;
	
	// Traverse all the item to count
	list_for_each_safe(pos,head,&message_head){
		count++;
	}
	
	return count;
}

static long device_ioctl(struct file *file,	/* see include/linux/fs.h */
		 unsigned int ioctl_num,	/* number and param for ioctl */
		 unsigned long ioctl_param)
{

	/* 
	 * Switch according to the ioctl called 
	 */
	if (ioctl_num == RESET_COUNTER) {
	    counter = 0; 
	    /* 	    return 0; */
	    return 5; /* can pass integer as return value */
	}

	else {
	    /* no operation defined - return failure */
	    return -EINVAL;

	}
}


/*
 * This function is called when the module is loaded
 */
int init_module(void)
{
    Major = register_chrdev(0, DEVICE_NAME, &fops);

	if (Major < 0) {
	  printk(KERN_ALERT "Registering char device failed with %d\n", Major);
	  return Major;
	}

	printk(KERN_INFO "I was assigned major number %d. To talk to\n", Major);
	printk(KERN_INFO "the driver, create a dev file with\n");
	printk(KERN_INFO "'mknod /dev/%s c %d 0'.\n", DEVICE_NAME, Major);
	printk(KERN_INFO "Try various minor numbers. Try to cat and echo to\n");
	printk(KERN_INFO "the device file.\n");
	printk(KERN_INFO "Remove the device file and module when done.\n");
	
	// init head
	INIT_LIST_HEAD(&message_head);
	//printk(KERN_INFO "[Init] Success.\n");

	return SUCCESS;
}

/*
 * This function is called when the module is unloaded
 */
void cleanup_module(void)
{
	ListOfMessage *pos, *temp;
	
	// delete and free all nodes
	list_for_each_entry_safe(pos,temp,&message_head, list){
		list_del(&pos->list);
		kfree(pos);
	}

	/*  Unregister the device */
	unregister_chrdev(Major, DEVICE_NAME);
	//printk(KERN_INFO "[cleanup] success.\n");
}

/*
 * Methods
 */

/* 
 * Called when a process tries to open the device file, like
 * "cat /dev/mycharfile"
 */
static int device_open(struct inode *inode, struct file *file)
{
    mutex_lock (&devLock);
    if (Device_Open) {
		mutex_unlock (&devLock);
		//printk(KERN_ALERT "open fail. busy. Device_Open = %d\n", Device_Open);
		return -EBUSY;
    }
    
    Device_Open++;
    mutex_unlock (&devLock);
    sprintf(msg, "I already told you %d times Hello world!\n", counter++);
    try_module_get(THIS_MODULE);

    //printk(KERN_INFO "[open] success. ");
    return SUCCESS;
}

/* Called when a process closes the device file. */
static int device_release(struct inode *inode, struct file *file)
{
    mutex_lock (&devLock);
	Device_Open--;		/* We're now ready for our next caller */
	mutex_unlock (&devLock);
	/* 
	 * Decrement the usage count, or else once you opened the file, you'll
	 * never get get rid of the module. 
	 */
	module_put(THIS_MODULE);

	//printk(KERN_INFO "[release] success. ");
	return 0;
}

/* 
 * Called when a process, which already opened the dev file, attempts to
 * read from it.
 */
static ssize_t device_read(struct file *filp,	/* see include/linux/fs.h   */
			   char *buffer,	/* buffer to fill with data */
			   size_t length,	/* length of the buffer     */
			   loff_t * offset)
{
	/* result of function calls */
	int result;
	ListOfMessage *pos, *head;

	// if list is empty return -EAGAIN
	if (list_empty(&message_head)){
	    //printk(KERN_ALERT "[read] fail. Empty list.\n");
	    return -EAGAIN;
	}

	// get the first avaliable node
	list_for_each_entry_safe(pos,head,&message_head,list){
		//If the present head is locked, move to next one
		if(mutex_trylock(&pos->bufflock) == 1){

			//Assign the message to msg
			sprintf(msg, "%s", pos->message);

			//Delete head from the list
			mutex_lock(&listLock);
			list_del(&pos->list);
			mutex_unlock(&listLock);

			//Unlock before free
			mutex_unlock(&pos->bufflock);
			kfree(pos);

			/* 
	 		 * Actually put the data into the buffer 
	 		 */

			if (strlen(msg) + 1 < length)
	    			length = strlen(msg) + 1;
			result = copy_to_user(buffer, msg, length);
			if (result > 0) 
	    			return -EFAULT; /* copy failed */

			/* 
	 		 * Most read functions return the number of bytes put into the buffer
	 		 */
			//printk(KERN_INFO "[read] success. Content is %s", buffer);
			return (length);
		}
	}
	
	//printk(KERN_ALERT "[read] fail. Empty list\n");
	return -EAGAIN;
}

/* Called when a process writes to dev file: echo "hi" > /dev/hello  */
static ssize_t
device_write(struct file *filp, const char *buff, size_t len, loff_t * off)
{
	int result;
	ListOfMessage *temp;
	
	// if buff longer than 4095+1(\0) then return -EINVAL
	if (BUF_LEN < len + 1){
	    //printk(KERN_ALERT "[write] Message is too long. Length is %ld\n",len+1);
	    return -EINVAL;
	}
	
	// if items in list larger than 1000 then return -EBUSY
	if (lengthoflist() >= 1000){
	    //printk(KERN_ALERT "[write] Message is full. Current size is %d\n",lengthoflist());
	    return -EBUSY;
	}
	
	// init a list node and the mutex lock
	temp = kmalloc(sizeof(ListOfMessage), GFP_KERNEL);
	mutex_init(&temp->bufflock);

	mutex_lock(&temp->bufflock);
	result = copy_from_user(temp->message,buff,len);
	if (result > 0) 
	    return -EFAULT; /* copy failed */
	INIT_LIST_HEAD(&temp->list);
	mutex_unlock(&temp->bufflock);
	
	mutex_lock(&listLock);
	list_add_tail(&temp->list, &message_head);
	mutex_unlock(&listLock);
	
	//printk(KERN_INFO "[write] success. Content is %s", temp->message);
	return len;
}
