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

typedef struct list_message{
	char message[4*1024];
	struct list_head list;
}ListOfMessage;

DEFINE_MUTEX  (devLock);
static int counter = 0;
struct list_head message_head;

int lengthoflist(void){
	int count = 0;
	struct list_head *pos, *head;
	
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

	// init a list head
	INIT_LIST_HEAD(&message_head);
	
	return SUCCESS;
}

/*
 * This function is called when the module is unloaded
 */
void cleanup_module(void)
{
	// delete nodes
	ListOfMessage *pos, *temp;
	list_for_each_entry_safe(pos,temp,&message_head, list){
		list_del(&pos->list);
		kfree(pos);
	}
	
	/*  Unregister the device */
	unregister_chrdev(Major, DEVICE_NAME);
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
	return -EBUSY;
    }
    Device_Open++;
    mutex_unlock (&devLock);
    sprintf(msg, "I already told you %d times Hello world!\n", counter++);
    try_module_get(THIS_MODULE);
    
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
	ListOfMessage *temp;

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
	 
	if (list_empty(&message_head))
	    return -EAGAIN;
	
	temp = list_first_entry(&message_head, ListOfMessage, list);
	printk(KERN_ALERT "%s", temp->message);
	list_del(&temp->list);
	kfree(temp);
	
	return SUCCESS;
}

/* Called when a process writes to dev file: echo "hi" > /dev/hello  */
static ssize_t
device_write(struct file *filp, const char *buff, size_t len, loff_t * off)
{
	ListOfMessage *temp;

	if (strlen(msg) + 1 < len)
	    len = strlen(msg) + 1;
	else
	    return -EINVAL;
	
	if (lengthoflist() >= 1000)
	    return -EBUSY;
	
	// init a list node
	temp = kmalloc(sizeof(*temp), GFP_KERNEL);
	strcpy(temp->message,buff);
	INIT_LIST_HEAD(&temp->list);
	
	// add to list
	list_add_tail(&temp->list, &message_head);
	
	return SUCCESS;
}
