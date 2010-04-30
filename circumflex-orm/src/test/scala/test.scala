package ru.circumflex.orm

class Country extends Record[Country] {
  def this(code: String, name: String) = {
    this()
    this.code := code
    this.name := name
  }
  val code = "code" VARCHAR(2) DEFAULT("'ch'")
  val name = "name" TEXT
  override def toString = name.getOrElse("Unknown")
}

object Country extends Table[Country] {
  INDEX("country_code_idx", "LOWER(code)") USING "btree" UNIQUE
}

class City extends Record[City] {
  def this(country: Country, name: String) = {
    this()
    this.country := country
    this.name := name
  }
  val name = "name" TEXT
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE ON_UPDATE CASCADE
  override def toString = name.getOrElse("Unknown")
}

object City extends Table[City]

class Capital extends Record[Capital] {
  def this(country: Country, city: City) = {
    this()
    this.country := country
    this.city := city
  }
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE
  val city = "city_id" REFERENCES(City) ON_DELETE RESTRICT
}

object Capital extends Table[Capital] {
  CONSTRAINT("capital_uniq") UNIQUE (this.country, this.city)
}

object Init {
  def schema = new DDLUnit(City, Capital, Country).dropCreate
      .messages.foreach(msg => println(msg.body))
  def data = {
    val ru = new Country("ru", "Russia")
    ru.save()
    val ch = new Country("ch", "Switzerland")
    ch.save()
    val msk = new City(ru, "Moscow")
    msk.save()
    val spb = new City(ru, "St. Petersburg")
    spb.save()
    val qrsk = new City(ru, "Kursk")
    qrsk.save()
    new Capital(ru, msk).save()
    val zurich = new City(ch, "Zurich")
    zurich.save()
    val bern = new City(ch, "Bern")
    bern.save()
    val lausanne = new City(ch, "Lausanne")
    lausanne.save()
    new Capital(ch, bern).save()
    COMMIT
  }
}
