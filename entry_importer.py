#!/usr/bin/python
#-*- coding: utf-8 -*-

import os, sys, datetime
from pymongo import Connection

basedir = sys.argv[1]

class Entry:

    def __init__(self, title, slug, content, tags, ctime, mtime):
        self.title = title
        self.slug = slug
        self.content = content
        self.tags = tags
        self.ctime = ctime
        self.mtime = mtime

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return self.__unicode__().encode('utf-8')

    def __unicode__(self):
        return u'Title: %s, Slug: %s, Body: %s..., Tags: %s, CTime: %s, MTime: %s'%(self.title, self.slug, self.content[:10], ','.join(self.tags), self.ctime, self.mtime)

    def as_dict(self):
        return {'title':self.title.encode('utf-8'),
                'content':self.content.encode('utf-8'),
                'slug':self.slug,
                'tags':self.tags,
                'creationDate':toTime(self.ctime),
                'updateDate':toTime(self.mtime),
                'language':'tr_TR',
                'format':'html'}

def toTime(sec):
    return datetime.datetime.fromtimestamp(sec)

def slugify(s):
    return os.path.splitext(s)[0].replace('_', '-', )

def import_entry(tags, slug, fn):
    stat = os.stat(fn)
    f = open(fn, 'rt')
    title = unicode(f.readline().strip(), encoding='utf-8')
    body = unicode(f.read(), encoding='utf-8')
    f.close()
    return Entry(title, slug, body, tags, stat.st_ctime, stat.st_mtime)

def import_entries(root, tags, entries):
    return [import_entry(tags, slugify(entry), os.path.join(root, entry)) for entry in entries]

entities = []
for root, dirs, files in os.walk(basedir, topdown=False):
    tags = root[len(basedir):].split(os.sep)
    entries = set([f for f in files if f.endswith('txt')])
    entities.extend(import_entries(root, tags, entries))

client = Connection()
db = client.reblog
coll = db.entries

for entry in entities:
    coll.insert(entry.as_dict())
