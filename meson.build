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
warning('sdfdsf',[1 ,2],'end')
set_variable('bb', 2)
unset_variable('bb')
libsystemd_version = '0.37.0'
libudev_version = '1.7.7'
libudev = 20
# libudev += 2
message(libudev)
message({'build mode' : get_option('mode')})

message(build_machine.cpu())
message(build_machine.endian())
message(meson.project_license())
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
assert(1 in [2, 3, 4])
subdir('meson')
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

summary({'Some boolean': false,
        'Another boolean': true,
        'Some string': 'Hello World',
        'A list': ['string', 1, true],
        },
        section: 'Configuration')

if add_languages('cpp', required : false)
  executable('cpp-app', 'main.cpp')
endif
executable('d','d.c',install : true)
progs = [['prog1', ['prog1.c', 'foo.c']],
         ['prog2', ['prog2.c', 'bar.c']]]
fdsfsdf=range(20)
gnome=import('gnome')
c='sdfsdf@fdsfsdf@'
kkk=c.format('txt')
b=gnome.genmarshal(
    'signon-marshal',
    sources: 'signon-marshal.list',
    prefix: '_signon_marshal'
)
env = environment()
# MY_PATH will be '0:1:2:3'
env.set('MY_PATH', '1')
env.append('MY_PATH', '2')
env.append('MY_PATH', '3')
env.prepend('MY_PATH', '0')
