import os
import subprocess
import string
import os.path
import fileinput
import sys
import vim
import time
import logging
from traceback import format_exc
from collections import defaultdict

# global vim config variables used (all are g:autotag<name>):
# name purpose
# maxTagsFileSize a cap on what size tag file to strip etc
# ExcludeSuffixes suffixes to not ctags on
# VerbosityLevel logging verbosity (as in Python logging module)
# CtagsCmd name of ctags command
# TagsFile name of tags file to look for
# Disabled Disable autotag (enable by setting to any non-blank value)
# StopAt stop looking for a tags file (and make one) at this directory (defaults to $HOME)
vim_global_defaults = dict(maxTagsFileSize = 1024*1024*7,
                           ExcludeSuffixes = "tml.xml.text.txt",
                           VerbosityLevel = logging.INFO,
                           CtagsCmd = "ctags",
                           TagsFile = ".ctags",
                           Disabled = 0,
                           StopAt = "/home/kevin")

def do_cmd(cmd, cwd):
   p = subprocess.Popen(cmd, shell=True, stdout=None, stderr=None, cwd=cwd)

def vim_global(name, kind = string):
   ret = vim_global_defaults.get(name, None)
   try:
      v = "g:autotag%s" % name
      exists = (vim.eval("exists('%s')" % v) == "1")
      if exists:
         ret = vim.eval(v)
      else:
         if isinstance(ret, int):
            vim.command("let %s=%s" % (v, ret))
         else:
            vim.command("let %s=\"%s\"" % (v, ret))
   finally:
      if kind == bool:
         ret = (ret not in [0, "0"])
      elif kind == int:
         ret = int(ret)
      elif kind == string:
         pass
      return ret

class VimAppendHandler(logging.Handler):
   def __init__(self, name):
      logging.Handler.__init__(self)
      self.__name = name
      self.__formatter = logging.Formatter()

   def __findBuffer(self):
      for b in vim.buffers:
         if b and b.name and b.name.endswith(self.__name):
            return b

   def emit(self, record):
      b = self.__findBuffer()
      if b:
         b.append(self.__formatter.format(record))

def makeAndAddHandler(logger, name):
   ret = VimAppendHandler(name)
   logger.addHandler(ret)
   return ret


class AutoTag:
   MAXTAGSFILESIZE = long(vim_global("maxTagsFileSize"))
   DEBUG_NAME = "autotag_debug"
   LOGGER = logging.getLogger(DEBUG_NAME)
   logging.basicConfig(filename='/home/kevin/autotag.log')
   HANDLER = makeAndAddHandler(LOGGER, DEBUG_NAME)

   @staticmethod
   def setVerbosity():
      try:
         level = int(vim_global("VerbosityLevel"))
      except:
         level = vim_global_defaults["VerbosityLevel"]
      AutoTag.LOGGER.setLevel(level)

   def __init__(self):
      self.tags = defaultdict(list)
      self.excludesuffix = [ "." + s for s in vim_global("ExcludeSuffixes").split(".") ]
      AutoTag.setVerbosity()
      self.sep_used_by_ctags = '/'
      self.ctags_cmd = vim_global("CtagsCmd")
      self.tags_file = str(vim_global("TagsFile"))
      self.count = 0
      self.stop_at = vim_global("StopAt")

   def findTagFile(self, source):
      AutoTag.LOGGER.info('source = "%s"', source)
      ( drive, file ) = os.path.splitdrive(source)
      ret = None
      while file:
         file = os.path.dirname(file)
         AutoTag.LOGGER.info('drive = "%s", file = "%s"', drive, file)
         tagsDir = os.path.join(drive, file)
         tagsFile = os.path.join(tagsDir, self.tags_file)
         AutoTag.LOGGER.info('tagsFile "%s"', tagsFile)
         if os.path.isfile(tagsFile):
            st = os.stat(tagsFile)
            if st:
               size = getattr(st, 'st_size', None)
               if size is None:
                  AutoTag.LOGGER.warn("Could not stat tags file %s", tagsFile)
                  break
               if size > AutoTag.MAXTAGSFILESIZE:
                  AutoTag.LOGGER.info("Ignoring too big tags file %s", tagsFile)
                  break
            ret = (file, tagsFile)
            break
         elif tagsDir and tagsDir == self.stop_at:
            AutoTag.LOGGER.info("Reached %s. Making one %s" % (self.stop_at, tagsFile))
            open(tagsFile, 'wb').close()
            ret = (file, tagsFile)
            break
         elif not file or file == os.sep or file == "//" or file == "\\\\":
            AutoTag.LOGGER.info('bail (file = "%s")' % (file, ))
            break
      return ret

   def addSource(self, source):
      if not source:
         AutoTag.LOGGER.warn('No source')
         return
      if os.path.basename(source) == self.tags_file:
         AutoTag.LOGGER.info("Ignoring tags file %s", self.tags_file)
         return
      (_, suff) = os.path.splitext(source)
      if suff in self.excludesuffix:
         AutoTag.LOGGER.info("Ignoring excluded suffix %s for file %s", source, suff)
         return
      found = self.findTagFile(source)
      if found:
         tagsDir, tagsFile = found
         relativeSource = os.path.splitdrive(source)[1][len(tagsDir):]
         if relativeSource[0] == os.sep:
            relativeSource = relativeSource[1:]
         if os.sep != self.sep_used_by_ctags:
            relativeSource = string.replace(relativeSource, os.sep, self.sep_used_by_ctags)
         self.tags[(tagsDir, tagsFile)].append(relativeSource)

   @staticmethod
   def goodTag(line, excluded):
      if line[0] == '!':
         return True
      else:
         f = string.split(line, '\t')
         AutoTag.LOGGER.debug("read tags line:%s", str(f))
         if len(f) > 3 and f[1] not in excluded:
            return True
      return False

   def stripTags(self, tagsFile, sources):
      AutoTag.LOGGER.info("Stripping tags for %s from tags file %s", ",".join(sources), tagsFile)
      backup = ".SAFE"
      input = fileinput.FileInput(files=tagsFile, inplace=True, backup=backup)
      try:
         for l in input:
            l = l.strip()
            if goodTag(l, sources):
	       print l
	    else:
	       AutoTag.LOGGER.debug("Delete line: %s", l)
      finally:
         input.close()
         try:
            os.unlink(tagsFile + backup)
         except StandardError:
            pass

   def updateTagsFile(self, tagsDir, tagsFile, sources):
      self.stripTags(tagsFile, sources)
      if self.tags_file:
         cmd = "%s -f %s -a " % (self.ctags_cmd, self.tags_file)
      else:
         cmd = "%s -a " % (self.ctags_cmd,)
      for source in sources:
         if os.path.isfile(os.path.join(tagsDir, source)):
            cmd += " '%s'" % source
      AutoTag.LOGGER.debug("%s: %s", tagsDir, cmd)
      for l in do_cmd(cmd, tagsDir):
         AutoTags.LOGGER.info("ctags: %s", l)

   def rebuildTagFiles(self):
      for ((tagsDir, tagsFile), sources) in self.tags.items():
         self.updateTagsFile(tagsDir, tagsFile, sources)

def autotag():
   try:
      if not vim_global("Disabled", bool):
         at = autotag.AutoTag()
         at.addSource(vim.eval("expand(\"%:p\")"))
         at.rebuildTagFiles()
   except:
      logging.warning(format_exc())

