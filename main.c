#include <htc.h>
#define xt
#define wdtdis
#define pwten

/*

Following description is copied from "http://www.pjrc.com/tech/8051/ide/wesley.html"

The IDE device appears as the following registers:

/CS0=0, /CS1=1, A2=A1=A0=0: data I/O register (16-bits) This is the data I/O register.
This is in fact the only 16-bits wide register of the entire IDE interface. It is used 
for all data block transfers from and to the IDE device.

/CS0=0, /CS1=1, A2..A0=001B: This is the error information register when read; the write 
precompensation register when written. I have never bothered with the write precompensation 
at all, I only read this register when an error is indicated in the IDE status register 
(see below for the IDE status register).

/CS0=0, /CS1=1, A2..A0=010B: Sector counter register. This register could be used to make 
multi-sector transfers. You'd have to write the number of sectors to transfer in this register. 
I use one-sector only transfers; So I'll only write 01H into this register. I do use this register
 to pass the parameter the timeout for idle modus command via this register.

/CS0=0, /CS1=1, A2..A0=011B: Start sector register. This register holds the start sector 
of the current track to start reading/ writing to. For each transfer I write the start 
sector in this register. For some fancy reason the sector count starts at 1 and runs up 
to 256, so writing 00H results in sector number 256. Why that is done is a mystery to 
me, all other counting in the IDE interface starts at 0.....

/CS0=0, /CS1=1, A2..A0=100B: Low byte of the cylinder number. This register holds low 
byte of the cylinder number for a disk transfer.

/CS0=0, /CS1=1, A2..A0=101B: High two bits of the cylinder number. The traditional 
IDE interface allows only cylinder numbers in the range 0..1023. This register gets 
the two upper bits of this number. I write the cylinder number's upper two bits into 
this register before each transfer.

/CS0=0, /CS1=1, A2..A0=110B: Head and device select register. The bits 3..0 of this register 
hold the head number (0..15) for a transfer. The bit 4 is to be written 0 for access to the 
IDE master device, 1 for access to the IDE slave device. The bits 7..5 are fixed at 101B in
 the traditional interface.

/CS0=0, /CS1=1, A2..A0=111B: command/status register. When written the IDE device regards 
the data you write to this register as a command. When read you get the status of the IDE 
device. Reading his register also clears any interrupts from the IDE device to the controller.

/CS0=1, /CS1=0, A2..A0=110B: 2nd status register/interrupt and reset register. When read 
this register gives you the same status byte as the primary (/CS0=0, /CS1=1, A2..A0=111B) 
status register. The only difference is that reading this register does not clear the 
interrupt from the IDE device when read. When written you can enable/disable the interrupts the 
IDE device generates; Also you can give a software reset to the IDE device.

/CS0=1, /CS1=0, A2..A0=111B: active status of the IDE device. In this register (read-only) 
you can find out if the IDE master or slave is currently active and find the currently selected 
head number. In a PC environment you can also find out if the floppy is currently in the drive. 
I have not used this register yet.

Some of these registers have bitwise meanings. I'll elaborate on that here: Back to contents

head and device register:
A write register that sets the master/slave selection and the head number.

bits 3..0: head number [0..15]
bit  4   : master/slave select: 0=master,1=slave
bits 7..5: fixed at 101B. This is in fact the bytes/sector
           coding. In old (MFM) controllers you could specify if
           you wanted 128,256,512 or 1024 bytes/sector. In the
           IDE world only 512 bytes/sector is supported. This bit
           pattern is a relic from the MFM controllers age. The
           bit 6 of this pattern could in fact be used to access
           a disk in LBA modus.

Status register:
Both the primary and secondary status register use the same bit coding. The register is a read register.

bit 0    : error bit. If this bit is set then an error has
           occurred while executing the latest command. The error
           status itself is to be found in the error register.
bit 1    : index pulse. Each revolution of the disk this bit is
           pulsed to '1' once. I have never looked at this bit, I
           do not even know if that really happens.
bit 2    : ECC bit. if this bit is set then an ECC correction on
           the data was executed. I ignore this bit.
bit 3    : DRQ bit. If this bit is set then the disk either wants
           data (disk write) or has data for you (disk read).
bit 4    : SKC bit. Indicates that a seek has been executed with
           success. I ignore this bit.
bit 5    : WFT bit. indicates a write error has happened. I do
           not know what to do with this bit here and now. I've
           never seen it go active.
bit 6    : RDY bit. indicates that the disk has finished its
           power-up. Wait for this bit to be active before doing
           anything (execpt reset) with the disk. I once ignored
           this bit and was rewarded with a completely unusable
           disk.
bit 7    : BSY bit. This bit is set when the disk is doing
           something for you. You have to wait for this bit to
           clear before you can start giving orders to the disk.

Back to contents

interrupt and reset register:

This register has only two bits that do something (that I know of). It is a write register.

bit 1    : IRQ enable. If this bit is '0' the disk will give and
           IRQ when it has finished executing a command. When it
           is '1' the disk will not generate interrupts.
bit 2    : RESET bit. If you pulse this bit to '1' the disk will
           execute a software reset. The bit is normally '0'. I
           do not use it because I have full software control of
           the hardware /RESET line.

Active status register:
This is a read register. I have -up till now- ignored this register. I have only one IDE device 
(a disk) on my contraption.

bit 0    : master active. If this bit is set then the master IDE
           device is active.
bit 1    : slave active. If this bit is set then the slave IDE
           device is active.
bits 5..2: complement of the currently active disk head.
bit 6    : write bit. This bit is set when the device is writing.
bit 7    : in a PC environment this bit indicates if a floppy is
           present in the floppy drive. Here it has no meaning.

error register:
The error register indicates what went wrong when a command execution results in an error. 
The fact that an error has occurred is indicated in the status register, the explanation is 
given in the error register. This is a read register.

bit 0    : AMNF bit. Indicates that an address mark was not
           found. What this means I not sure of. I have never
           seen this happen.
bit 1    : TK0NF bit. When this bit is set the drive was not able
           to find track 0 of the device. I think you will have
           to throw away the disk if this happens.
bit 2    : ABRT bit. This bit is set when you have given an
           indecent command to the disk. Mostly wrong parameters
           (wrong head number etc..) cause the disk to respond
           with error flag in the status bit set and the ABRT bit
           set. I have gotten this error a lot when I tried to
           run the disk with interrupts. Something MUST have been
           wrong with my interface program. I have not (yet)
           found what.
bit 3    : MCR bit. indicated that a media change was requested.
           What that means I do not know. I've ignored this bit
           till now.
bit 4    : IDNF bit. Means that a sector ID was not found. I have
           never seen this happen, I guess it means that you've
           requested a sector that is not there.
bit 5    : MC bit. Indicates that the media has been changed. I
           ignore this bit.
bit 6    : UNC bit. Indicates that an uncorrectable data error
           happened. Some read or write errors could provoke
           this. I have never seen it happen.
bit 7    : reserved.

IDE command:    Description:
------------    ------------

1XH             recalibrate the disk. NB: 1XH means that the lower
                nibble of the command byte is a don't care. All
                commands 10H..1FH will result in a recalibrate
                disk command being executed. This command has no
                parameters. You simply write the command code to
                the command register and wait for ready status to
                become active again.

20H             Read sector with retry. NB: 21H = read sector
                without retry. For this command you have to load
                the complete circus of cylinder/head/sector
                first. When the command completes (DRQ goes
                active) you can read 256 words (16-bits) from the
                disk's data register.

30H             Write sector (with retry; 31H = without retry).
                Here too you have to load cylinder/head/sector.
                Then wait for DRQ to become active. Feed the disk
                256 words of data in the data register. Next the
                disk starts writing. When BSY goes not active you
                can read the status from the status register.

7XH             Seek. This normally does nothing on modern IDE
                drives. Modern drives do not position the head if
                you do not command a read or write.

ECH             Identify drive. This command prepares a buffer
                (256 words) with information about the drive. If
                you want the details either look closely at the
                interface program I will add at the end of this
                description or get the ATA-3 document. To use it:
                simply give the command, wait for DRQ and read
                the 256 words from the drive. I have found that
                the modern drives I used give nice information
                about number of heads,sectors,cylinders etc...
                One of the disks I tried (a Miniscribe 8051A)
                gave wrong answers in this buffer. The disk is
                actually a 4 heads/28 sectors disk. It should be
                used in a translated modus with 5 heads/17
                sectors. In the ident drive response it reported
                as 4 heads/28 sectors and it will NOT work in
                that modus. Two other disks (a Quantum 127 MB
                disk and a Western Digital 212 MB disk) report
                nicely. If not for the Miniscribe I would use the
                parameters reported to auto-config the interface
                to match the disk configuration.

E0H             Spins down the drive at once. No parameters. My
                Miniscribe 8051A does not respond to this
                command, the other disks do execute this command.

E1H             Spins up the drive again. Same remarks as E0H
                command.

E2H and E3H     Auto-power-down the disk. Write in the sector
                count register the time (5 seconds units) of
                non-activity after which the disk will spin-down.
                Write the command to the command register and the
                disk is set in an auto-power-save modus. The disk
                will automatically spin-up again when you issue
                read/write commands. E2H will spin-down, E3H will
                keep the disk spinning after the command has been
                given. Example: write 10H in the sector count
                register, give command E2H and the disk will
                spin-down after 80 seconds of non-activity. BTW:
                You can use this command easily on a PC disk too.
                The harddisk of the computer I am working on now
                gets this exact command at boot. That saves a lot
                of noise when I'm typing long stories like this
                one.

F2H and F3H     The same as E2H and E3H, only the unit in the
                sector count register is interpreted as 0.1 sec
                units. I have not tried this command. I think it
                will work (the E0H/E1H/E2H/E3H commands work, why
                should this one not work?)

end of copy

81C55 ASSIGNMENT:
----------------

PA0..7	= IDE LOW BYTE
PB0..7	= IDE HIGH BYTE

PC.0	= A0
PC.1	= A1
PC.2	= A2
PC.3	= !CS1/CS0
PC.4	= !RD
PC.5	= !WR

8 4 2 1 - 8 4 2 1
*/

#define FALSE 0
#define TRUE 1

#define	PORTBIT(adr, bit)	((unsigned)(&adr)*8+(bit))

static bit	TR 	 @	PORTBIT(PORTA, 0);
static bit	CE 	 @	PORTBIT(PORTA, 1);
static bit	IORD @	PORTBIT(PORTA, 2);
static bit	ALE  @	PORTBIT(PORTA, 3);
static bit	IOWR @	PORTBIT(PORTA, 4);


void move_to_8155(unsigned char DATA, unsigned char ADR);
unsigned char get_from_8155(unsigned char ADR);
unsigned char mirror( unsigned char n );
void delay(unsigned char n);
void wait(int i);

void HDD_SPIN_DOWN(void);
void HDD_SPIN_UP(void);
unsigned long HDD_READ(unsigned int CYLINDER, unsigned char HEAD, unsigned char SECTOR);

unsigned char LOW_BYTE;
unsigned char HIGH_BYTE;

void main(void)
{
	unsigned char i;
	TRISA=0x00;	TRISB=0x00;
	PORTB=0xFF;
//	GIE=1;		T0IE=1;

	PS0=0;	PS1=0;	PS2=0;

//	T0CS=0;	T0SE=0;

	IORD = TRUE;
	IOWR = TRUE;
	ALE = TRUE;
	TR = TRUE;
	CE = TRUE;


	while(1)
	{
		HDD_SPIN_DOWN();
		wait(30000);	
		HDD_SPIN_UP();
		wait(30000);
		//HDD_READ(10,0,0);
		wait(30000);
	}

}

void HDD_SPIN_DOWN() 
{
	move_to_8155(0x0F, 0x00);	// Output mode 
	move_to_8155(0xFF, 0x03);	// PC = 0xFF	//CMD REG: /CS0=0, /CS1=1, A2..A0=111B
	move_to_8155(0x00, 0x02);	// PB = 0x00
	move_to_8155(0xE0, 0x01);	// PA = 0xE0 = spin down cmd
	move_to_8155(0xDF, 0x03);	// !WR = LOW
	move_to_8155(0xFF, 0x03);	// !WR = HIGH
}

void HDD_SPIN_UP() 
{
	move_to_8155(0x0F, 0x00);	// Output mode	
	move_to_8155(0xFF, 0x03);	// PC = 0xFF	//CMD REG: /CS0=0, /CS1=1, A2..A0=111B
	move_to_8155(0x00, 0x02);	// PB = 0x00
	move_to_8155(0xE1, 0x01);	// PA = 0xE1 = spin up cmd
	move_to_8155(0xDF, 0x03);	// !WR = LOW
	move_to_8155(0xFF, 0x03);	// !WR = HIGH
}

unsigned long HDD_READ(unsigned int CYLINDER, unsigned char HEAD, unsigned char SECTOR) 
{
	unsigned char LOW_BYTE_CYLINDER, HIGH_BYTE_CYLINDER;

	unsigned char LOW_BYTE, HIGH_BYTE;

	LOW_BYTE_CYLINDER &= CYLINDER;
	CYLINDER <= 4;
	HIGH_BYTE_CYLINDER &= CYLINDER;

	move_to_8155(0x0F, 0x00);	// Output mode 
	move_to_8155(0xFC, 0x03);	// PC = 0xFF	//CMD REG: /CS0=0, /CS1=1, A2..A0=100B = LOW BYTE CYLINDER = XX11 1 100
	move_to_8155(0x00, 0x02);	// PB = 0x00
	move_to_8155(LOW_BYTE_CYLINDER, 0x01);
	move_to_8155(0xDF, 0x03);	// !WR = LOW
	move_to_8155(0xFF, 0x03);	// !WR = HIGH

	move_to_8155(0xFD, 0x03);	// PC = 0xFF	//CMD REG: /CS0=0, /CS1=1, A2..A0=101B = HIGH BYTE CYLINDER = XX11 1 101
	move_to_8155(0x00, 0x02);	// PB = 0x00
	move_to_8155(HIGH_BYTE_CYLINDER, 0x01);
	move_to_8155(0xDF, 0x03);	// !WR = LOW
	move_to_8155(0xFF, 0x03);	// !WR = HIGH

	move_to_8155(0x0F, 0x00);	// Output mode 
	move_to_8155(0xFE, 0x03);	// PC = 0xFF	//CMD REG: /CS0=0, /CS1=1, A2..A0=110B = HEAD = XX11 1 110
	move_to_8155(0x00, 0x02);	// PB = 0x00
	move_to_8155(HEAD, 0x01);
	move_to_8155(0xDF, 0x03);	// !WR = LOW
	move_to_8155(0xFF, 0x03);	// !WR = HIGH

	move_to_8155(0x0F, 0x00);	// Output mode 
	move_to_8155(0xFC, 0x03);	// PC = 0xFF	//CMD REG: /CS0=0, /CS1=1, A2..A0=011B = LOW BYTE CYLINDER = XX11 1 011
	move_to_8155(0x00, 0x02);	// PB = 0x00
	move_to_8155(SECTOR, 0x01);
	move_to_8155(0xDF, 0x03);	// !WR = LOW
	move_to_8155(0xFF, 0x03);	// !WR = HIGH

	move_to_8155(0x0F, 0x00);	// Output mode 
	move_to_8155(0xFF, 0x03);	// PC = 0xFF	//CMD REG: /CS0=0, /CS1=1, A2..A0=100B = CMD REG = XX11 1 111
	move_to_8155(0x00, 0x02);	// PB = 0x00
	move_to_8155(0x20, 0x01);	// READ WITH RETRY
	move_to_8155(0xDF, 0x03);	// !WR = LOW
	move_to_8155(0xFF, 0x03);	// !WR = HIGH

	move_to_8155(0x0C, 0x00);	// Input mode 
	move_to_8155(0xFC, 0x03);	// PC = 0xFF	//CMD REG: /CS0=0, /CS1=1, A2..A0=100B = LOW BYTE CYLINDER = XX11 1 000
	move_to_8155(HIGH_BYTE, 0x02);	// PB = 0x00
	move_to_8155(LOW_BYTE, 0x01);
	move_to_8155(0xDF, 0x03);	// !WR = LOW
	move_to_8155(0xFF, 0x03);	// !WR = HIGH



	return 1;
}

void move_to_8155(unsigned char DATA, unsigned char ADR)
{
	TR	= TRUE;
	CE	= FALSE;
		delay(2);
	ALE = TRUE;
		delay(2);
	PORTB = mirror(ADR);
		delay(2);
	ALE = FALSE;
		delay(2);
	PORTB = mirror(DATA);
		delay(2);
	IOWR = FALSE;
		delay(2);
	IOWR = TRUE;
		delay(2);
	CE = TRUE;
		delay(2);
}

unsigned char get_from_8155(unsigned char ADR)
{
	unsigned char DATA;

	TR		= TRUE;				// PIC =>> LATCH =>> 8155

	CE		= FALSE;
		delay(2);
	ALE 	= TRUE;
		delay(2);
	PORTB 	= mirror(ADR);
		delay(2);
	ALE 	= FALSE;
		delay(2);

	TRISB 	= 0xFF;
		delay(2);

	TR		= FALSE;
		delay(2);
	DATA 	= mirror(PORTB);
		delay(2);
	IORD 	= FALSE;
		delay(2);
	IORD 	= TRUE;
		delay(2);
	CE 		= TRUE;
		delay(2);
	TR		= TRUE;

	return DATA;
}

unsigned char mirror( unsigned char n )
{
	n = ((n >> 1) & 0x55) | ((n << 1) & 0xaa);
	n = ((n >> 2) & 0x33) | ((n << 2) & 0xcc);
	n = ((n >> 4) & 0x0f) | ((n << 4) & 0xf0);
	return n;
}

void delay( unsigned char n)
{
	while(n--);
}

void wait( int i)
{
	while(i--)
	delay(200);
}