<h1>Snap Example App Login</h1>

<p><loginError/></p>

<bind tag="postAction">/login</bind>
<bind tag="submitText">Login</bind>
<apply template="userform"/>

<p>Don't have a login yet? <a href="/new_user">Create a new user</a></p>
<ol>
  <li>
    <a href="/oauth/weibo">Login with Weibo</a>
  </li>
  <li>
    <a href="/oauth/google">Login with Google</a>
  </li>
</ol>

