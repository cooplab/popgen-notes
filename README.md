# Population Genetics Notes

This is the latex source, images, and R code for the population genetics notes
by Graham Coop.  A fuller description of this resource is available at my
website http://gcbias.org/population-genetics-notes/

Theses notes, code, and all of the figures are released under the Creative
Commons Attribution 3.0 Unported License.So if you do reuse these you don’t
need my permission, you just need to acknowledge where oyu got them from. My
intention is that these notes can provide a resource for others to write their
own versions of these notes. 

There are still many typos in these notes, but I hope to fix them over time,
and feel free to contact me or leave a comment here.

I hope to develop these notes into a fuller resource over the coming years,
to create an up to date open source population genetics textbook.

There are various other popgen notes openly available online. Here’s a couple of other resources:

Kent Holsinger’s Notes: https://github.com/kholsinger/Lecture-Notes-in-Population-Genetics

Joe Felsenstein’s book: http://evolution.genetics.washington.edu/pgbook/pgbook.html

Waslh and Lynch pdfs: http://nitro.biosci.arizona.edu/zbook/NewVolume_2/newvol2.html

Graham

## Notes on Building the Site

You need latexmk and pandoc. Then use:

    $ make clean site

`.eps` images will not work — be sure to convert all `.eps` files to `.png` and
use the `.png` versions in LaTeX files.

    $ find  . -name '*.eps' -type f | xargs -n1 -I{} basename {} .eps | xargs -n1 -I{} convert {}.eps {}.png

Then to deploy, use:

    $ make deploy

This pushes `html/` with `git subtree`, e.g. like

    $ git subtree push --prefix html upstream gh-pages

For reference, [this](https://github.com/X1011/git-directory-deploy)
`deploy.sh` is used to deploy to the `gh-pages` branch. I've modified this
version accordingly (e.g. works with BSD `ls`, by using `-A` instead of
`--almost-all`)
