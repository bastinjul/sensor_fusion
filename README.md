# sensor_fusion
> Repository of the master thesis "sensor fusion" [@uclouvain](uclouvain.be) by Julien Bastin (julien.bastin@student.uclouvain.be) & Guillaume Neirinckx (guillaume.neirinckx@student.uclouvain.be)

## Network configuration

### Connect to boards via ADHOC network
You need to put in the [grisp.ini.mustache](./grisp/grisp_base/files/grisp.ini.mustache) file these lines behind `[network]`:

```
wlan=enable
{{#env.IP}}
ip_self={{env.IP}}
{{/env.IP}}
wlan_ip_netmask=255.255.0.0
wlan_mode=adhoc
wlan_adhocname=edge
wlan_channel=6
{{#env.NAME}}
hostname={{env.NAME}}
{{/env.NAME}}
```

### Connect to boards via wifi
You need to put in the [grisp.ini.mustache](./grisp/grisp_base/files/grisp.ini.mustache) file these lines behind `[network]`:

```
ip_self = dhcp
wlan = enable
{{#env.NAME}}
hostname = {{env.NAME}}
{{/env.NAME}}
wpa = wpa_supplicant.conf
```

You also need to modify the file [wpa_supplicant.conf](./grisp/grisp_base/files/wpa_supplicant.conf) and put the information relative to your network.

## Deployment
The command to deploy on the grisp is : 

```
NAME=<hostname> IP=X.X.X.X rebar3 grisp deploy -n sensor_fusion -v 0.1.0
``` 

If you are using Wifi network, you do not need to put the `IP=X.X.X.X`.