# -*- coding: utf-8; mode: sage -*-
from sage.repl.rich_output.output_catalog import OutputImagePng
from sage.repl.rich_output.backend_ipython import BackendIPythonCommandline
from sage.repl.rich_output import get_display_manager
from emacs_sage_shell import ip


class BackendEmacsBabel(BackendIPythonCommandline):

    def __init__(self):
        self.__emacs_babel_filename = None
        super(BackendEmacsBabel, self).__init__()

    def _emacs_babel_filename(self):
        return self.__emacs_babel_filename

    def _set_emacs_babel_filename(self, filename):
        self.__emacs_babel_filename = filename

    def _repr_(self):
        return "Emacs babel"

    def displayhook(self, plain_text, rich_output):
        if isinstance(rich_output, OutputImagePng):
            msg = rich_output.png.filename(ext='png')
            babel_filename = self._emacs_babel_filename()
            if babel_filename is not None:
                rich_output.png.save_as(babel_filename)
                msg = babel_filename
            return ({u'text/plain': msg}, {})
        else:
            return super(BackendEmacsBabel, self).displayhook(plain_text, rich_output)


backend_ob_sage = BackendEmacsBabel()
gdm = get_display_manager()


def run_cell_babel(code, file_name=None):
    backend_ob_sage._set_emacs_babel_filename(file_name)
    prv = gdm.switch_backend(backend_ob_sage, shell=ip)
    try:
        ip.run_cell(code)
    finally:
        gdm.switch_backend(prv, shell=ip)
