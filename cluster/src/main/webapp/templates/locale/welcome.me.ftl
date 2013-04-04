[#ftl]
{.super-title.medium}
Welcome to Circumflex Cluster Manager!

[#if conf.users.children?size > 0]
Your cluster configuration appears to be intact.

Proceed to login to begin your work.

<div class="centered">
  <a href="/auth/login"
     class="btn primary inverse">Login</a>
</div>
[/#if]

### Configuring Cluster Manager

Create cluster configuration file at
<code>${conf.descriptorFile.getAbsolutePath}</code>
to begin your work.

``` {.hl.xml}
<?xml version="1.0"?>
<cluster>
  <!-- Place authentication data for administrators here -->
  <users>
    <user name="admin"
          passwdSha256="sha-256 of account password"/>
  </users>
  <!-- Configure maven projects for managed cluster build here -->
  <projects>
    <project path="/path/to/your/project"/>
  </projects>
</cluster>
```