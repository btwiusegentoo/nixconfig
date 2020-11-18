{
  enable = true;
  fan = "tp_fan /proc/acpi/ibm/fan";
  sensors = ''
    hwmon /sys/class/thermal/thermal_zone0/temp
  '';
  levels = ''
    (0, 0,  42)
    (1, 40, 47)
    (2, 45, 52)
    (3, 50, 57)
    (4, 55, 62)
    (5, 60, 72)
    (6, 65, 77)
    (7, 70, 80)
    (127, 75, 32767)
  '';
}
