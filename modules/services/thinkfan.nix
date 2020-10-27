{
    enable = true;
    fan = "tp_fan /proc/acpi/ibm/fan";
    sensors = ''
                hwmon /sys/class/thermal/thermal_zone0/temp
            '';
    levels = ''
                (0, 0,  42)
                (1, 36, 46)
                (2, 45, 55)
                (3, 54, 60)
                (4, 59, 68)
                (5, 67, 75)
                ("level full-speed", 60, 32767)
            '';
}
