<apply template="base">

  <ifLoggedIn>
    <ul class="nav nav-pills">
      <li><a href="/experiment">Begin</a></li>
      <li><a href="/results">Results</a></li>
      <li><a href="/logout">Logout</a></li>
    </ul>

    <p class="text-center">&uarr; - enlarge, &darr; - shrink</p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
