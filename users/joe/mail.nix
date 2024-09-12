# Joe's dotfiles
# Copyright (C) 2023  Josep Jesus Bigorra Algaba (jjbigorra@gmail.com)

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

{ pkgs, ... }:

{
  services.mbsync = {
    enable = true;
    frequency = "*:0/3";
    postExec = "${pkgs.mu}/bin/mu index";
  };

  home.file.".mbsyncrc".text = ''
    IMAPAccount gmail
    Host imap.gmail.com
    User jjbigorra@gmail.com
    PassCmd "/etc/profiles/per-user/joe/bin/cat ~/.my-gmail-pass"
    AuthMechs LOGIN
    SSLType IMAPS
    SSLVersions TLSv1.2
    CertificateFile /etc/ssl/certs/ca-certificates.crt

    IMAPStore Gmail-remote
    Account gmail

    MaildirStore Gmail-local
    Subfolders Verbatim
    Path ~/Mail/jjbigorra@gmail.com/
    Inbox ~/Mail/jjbigorra@gmail.com/Inbox



    Channel Gmail-inbox
    Far :Gmail-remote:"INBOX"
    Near :Gmail-local:"INBOX"
    CopyArrivalDate yes
    Create Both
    Expunge Both
    SyncState *

    Channel Gmail-trash
    Far :Gmail-remote:"[Gmail]/Prullenbak"
    Near :Gmail-local:"Prullenbak"
    CopyArrivalDate yes
    Create Both
    Expunge Both
    SyncState *

    Channel Gmail-spam
    Far :Gmail-remote:"[Gmail]/Spam"
    Near :Gmail-local:"Spam"
    CopyArrivalDate yes
    Create Both
    Expunge Both
    SyncState *
        
    Channel Gmail-all
    Far :Gmail-remote:"[Gmail]/Alle e-mail"
    Near :Gmail-local:"Archive"
    CopyArrivalDate yes
    Create Both
    Expunge Both
    SyncState *

    Channel Gmail-drafts
    Far :Gmail-remote:"[Gmail]/Concepten"
    Near :Gmail-local:"Concepten"
    CopyArrivalDate yes
    Create Both
    Expunge Both
    SyncState *

    Group Gmail
    Channel Gmail-inbox
    Channel Gmail-trash
    Channel Gmail-all
    Channel Gmail-spam
    Channel Gmail-drafts
  '';
}

