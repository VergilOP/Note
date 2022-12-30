# Installing Haskell in your own machine

You don't need to install Haskell in your own machine, because you will be required to run Haskell using the Jupyter service that we provide.

But if you nevertheless would like to install it, here are
instructions.

## You **must** test your coursework with Jupyter before submission

Your coursework should work in Jupyter and you should never submit any piece of work that you haven't tested with Jypyter. We cannot accept submissions that don't compile or run in Jupyter, because we mark them by testing them there. If they don't pass our tests, we won't allocate marks to your submission, even if it works properly in your machine.

This is very important and **we cannot make any exceptions to this**. So make sure that if you choose to work with the Haskell ecosystem installed in your own machine, you thoroughly test your coursework in the Jupyter before you submit it.

## Support

Unfortunately we are not able to offer support for
self-installed software. However, feel free to ask in the [Teams
student support](https://teams.microsoft.com/l/team/19%3aF9Q4-aQD3xHR-h7kH-wtZlvs0Lh-KMZIHRdUZs45T5A1%40thread.tacv2/conversations?groupId=309bd71b-e5c7-432f-ba7c-c71fe6fffca2&tenantId=b024cacf-dede-4241-a15c-3c97d553e9f3) -
the lecturers or some other student or teaching assistant may be able to help.


## We need Haskell version 8.10.7

This is the version of Haskell available in Jupyter and the one we are going to use to mark your submissions.

## Windows, MacOS, Alpine, Debian, Fedora, CentOS, FreeBSD

You can download it [here](https://www.haskell.org/ghc/download_ghc_8_10_7.html)

## Ubuntu

In Ubuntu 20.04, the closest available version seems to be 8.10.4. You can install it from the terminal as follows:
```
$ sudo add-apt-repository ppa:hvr/ghc
$ sudo apt-get update
$ sudo apt install ghc-8.10.4
```
It should work, but, as explained above, any work should be tested on Jupyter before submission.
