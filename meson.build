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
libudev = 20
libudev += 2
message(libudev)
conf = configuration_data()


cdata = configuration_data({
  'STRING' : '"foo"',
  'INT' : 42,
  'DEFINED' : true,
  'UNDEFINED' : false,
})
assert(1 not in [2, 3, 4])

assert([1, 2, 3, 4, 5].contains(3))

arr_with_comments = [
    true,
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #

    true,
]
assert(arr_with_comments[1])
env = environment({'other_key': 'val', 'other_key2': 'val1'})
