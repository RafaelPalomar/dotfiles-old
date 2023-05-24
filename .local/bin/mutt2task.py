#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import email
import re
import errno
import shutil

from email.header import decode_header
from subprocess import Popen, PIPE, call

def rollback():
    print("INFO: Rolling back incomplete task/note creation:")
    call(['task', 'rc.confirmation=off', 'undo'])

home_dir = os.path.expanduser('~')

notes_folder = ""
notes_folder_pat = re.compile("^[^#]*\s*NOTES_FOLDER\s*=\s*(.*)$")
for line in open("%s/.taskopenrc" % home_dir, "r"):
    match = notes_folder_pat.match(line)
    if match:
        notes_folder = match.group(1).replace('"', '')

if "$HOME" in notes_folder:
    notes_folder = notes_folder.replace("$HOME", home_dir)

if not notes_folder:
    notes_folder = "%s/.tasknotes" % home_dir

try:
    os.mkdir(notes_folder, 0o750)
except OSError as ose:
    if ose.errno == errno.EEXIST and os.path.isdir(notes_folder):
        pass
    else:
        print("ERR: Sorry, cannot create directory \"%s\"." % notes_folder)
        raise

raw_message = sys.stdin.read()
message = email.message_from_string(raw_message)

body = None
html = None
for part in message.walk():
    if part.get_content_type() == "text/plain":
        if body is None:
            body = ""
        body += part.get_payload(decode=True).decode(
            part.get_content_charset() or 'utf8',
            'replace'
        )
    elif part.get_content_type() == "text/html":
        if html is None:
            html = ""
        html += part.get_payload(decode=True).decode(
            part.get_content_charset() or 'utf8',
            'replace'
        )

tmpfile = Popen('mktemp', stdout=PIPE).stdout.read().strip().decode()
out = ""
if html:
    with open(tmpfile, "w") as f:
        f.write(html)

    p1 = Popen(['cat', tmpfile], stdout=PIPE)
    p2 = Popen(['elinks', '--dump'], stdin=p1.stdout, stdout=PIPE)
    out = p2.stdout.read().decode()
else:
    out = body

with open(tmpfile, "w") as f:
    f.write(out)

subject = message['Subject']

# decode internationalized subject and transform ascii into utf8
subject = decode_header(subject)
subject = ' '.join([t[0].decode(t[1] or 'ASCII') if isinstance(t[0], bytes) else t[0] for t in subject])

# customize your own taskwarrior line
# use `subject` to add the subject
if subject == "None":
    subject = "E-Mail import: no subject specified."
else:
    subject = "E-Mail subject: " + subject

res = Popen(['task', 'add', 'pri:L', '+email', '--', subject], stdout=PIPE)
match = re.match("^Created task (\d+).*", res.stdout.read().decode())
if match:
    print(match.string.strip())
    id = match.group(1)
    uuid = Popen(['task', id, 'uuids'], stdout=PIPE).stdout.read().strip().decode()
    ret = call(['task', id, 'annotate', '--', 'email:', 'Notes'])
    if ret:
        print("ERR: Sorry, cannot annotate task with ID=%s." % id)
        rollback()

    notes_file = notes_folder + "/" + uuid + ".txt"
    try:
        shutil.copy(tmpfile, notes_file)
        os.remove(tmpfile)
    except:
        print("ERR: Sorry, cannot create notes file \"%s\"." % notes_file)
        rollback()
