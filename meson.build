# SPDX-License-Identifier: LGPL-2.1-or-later

project('systemd', 'c',
        version : '254',
        license : 'LGPLv2+',
        default_options: [
                'c_std=gnu11',
                'prefix=/usr',
                'sysconfdir=/etc',
                'localstatedir=/var',
                'warning_level=2',
        ],
        meson_version : '>= 0.60.0',
       )

libsystemd_version = '0.37.0'
libudev_version = '1.7.7'

conf = configuration_data()


cdata = configuration_data({
  'STRING' : '"foo"',
  'INT' : 42,
  'DEFINED' : true,
  'UNDEFINED' : false,
})
