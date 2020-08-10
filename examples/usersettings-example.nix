{
    service.openssh.ports = [ 22 ];    # Change to better port if you want
    time.timeZone = "America/Chicago"; # set your local timezone. See timedatectl list-timezones
    home.keyboard = {                  # Set your keyboard layout. I recommend Dvorak.
        layout = "us";
        variant = "dvorak";
    };
    # Scale 1080p monitor to WQHD.
    #xsession.profileExtra = "xrandr --output DVI-D-0 --scale 1.33333333333333x1.33333333333333 --panning 2560x1440 ";
}
