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
# libudev += 2
message(libudev)
meson_build_sh = find_program('meson.build', required : true)
conf = configuration_data()
conf.set('PROJECT_VERSION','20-12')
message(conf.get('PROJECT_VERSION'))
# cdata = configuration_data({
#   'STRING' : '"foo"',
#   'INT' : 42,
#   'DEFINED' : true,
#   'UNDEFINED' : false,
# })
assert(1 not in [2, 3, 4])

# assert([1, 2, 3, 4, 5].contains(3))
ccc= false or false or 20
message(ccc)
arr_with_comments = [
    true,
    ['abc'],
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
pk(arr_with_comments[1][0])
if libudev_version != ''
  pk('y')
else
  pk('n')
endif
c=1/2
joined = '/usr/share' / 'projectname'
pk('/abc'.startswith('/'))
# error('hh')
multiline_string = '''#include <foo.h>
int main (int argc, char ** argv) {
  return FOO_SUCCESS;
}'''

pk(multiline_string)
assert(( 5 % 2) == 2)
