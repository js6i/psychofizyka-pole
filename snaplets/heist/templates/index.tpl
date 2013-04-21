<apply template="base">

  <ifLoggedIn>
    <ul class="nav nav-pills">
      <li><a href="/experiment">Rozpocznij</a></li>
      <li><a href="/results">Wyniki</a></li>
      <li><a href="/logout">Wyloguj</a></li>
    </ul>

    <p>
      Eksperyment polega na zbadaniu subiektywnej oceny stosunku pól powierzchni pary figur geometrycznych (kół lub kwadratów).
      Zadaniem badanego jest ustawienie rozmiaru jednej z figur tak, aby stosunek ich pól był według jego oceny równy
      zadanemu stosunkowi docelowemu.
      Aby rozpocząć badanie naciśnij przycisk 'Rozpocznij'.
    </p>

  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
