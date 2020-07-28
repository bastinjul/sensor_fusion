# Format a sd card

> The tutorial below comes from these two web sites : https://www.cio.com/article/3176034/how-to-format-an-sd-card-in-linux.html  - https://askubuntu.com/questions/868894/how-to-name-an-sd-card

1.  Plug in your removable flash drive and run the `lsblk` command to identify the device.

2. Execute the following command :
`sudo parted /dev/mmcblk0`

3. Inside parted, enter the following commands :

`(parted) mklabel msdos`
`(parted) mkpart primary fat32 1MiB 100%`
`(parted) set 1 boot on`
`(parted) quit`

4. Now format it as fat32 :
`sudo mkfs.vfat /dev/mmcblk0p1`

5. Finally, change the name of your device :
`sudo mlabel -i /dev/mmcblk0p1 ::GRISP`