use regex::{Captures, Regex};
use std::collections::HashSet;
use std::io::{Error, ErrorKind, Result};

const REQUIRED_DOCUMENT_FIELDS: [Field; 7] = [
  Field::BirthYear,
  Field::IssueYear,
  Field::ExpirationYear,
  Field::Height,
  Field::HairColor,
  Field::EyeColor,
  Field::PassportId,
];

#[derive(Debug)]
pub struct Document {
  fields: HashSet<Field>,
}

impl Document {
  fn new() -> Self {
    Document {
      fields: HashSet::new(),
    }
  }

  fn add(&mut self, capture: &Captures) -> Result<()> {
    let name = self.field_from_str(capture.get(1).unwrap().as_str());
    match name {
      Some(field) => {
        self.fields.insert(field);
        Ok(())
      }
      None => Err(Error::new(
        ErrorKind::Other,
        format!("Error parsing input '{:?}'", capture),
      )),
    }
  }

  fn field_from_str(&self, s: &str) -> Option<Field> {
    match s {
      "byr" => Some(Field::BirthYear),
      "iyr" => Some(Field::IssueYear),
      "eyr" => Some(Field::ExpirationYear),
      "hgt" => Some(Field::Height),
      "hcl" => Some(Field::HairColor),
      "ecl" => Some(Field::EyeColor),
      "pid" => Some(Field::PassportId),
      "cid" => Some(Field::CountryId),
      _ => None,
    }
  }

  pub fn is_valid(&self) -> bool {
    REQUIRED_DOCUMENT_FIELDS.iter().fold(
      true,
      |acc, req| {
        if acc {
          self.fields.contains(req)
        } else {
          acc
        }
      },
    )
  }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Field {
  BirthYear,
  IssueYear,
  ExpirationYear,
  Height,
  HairColor,
  EyeColor,
  PassportId,
  CountryId, // We don't care about this
}

pub fn documents_from_input(input: Vec<String>) -> Result<Vec<Document>> {
  let documents = input.iter().fold(vec![Document::new()], |mut docs, line| {
    lazy_static! {
      static ref RE: Regex = Regex::new(r"([a-z]{3}):").unwrap();
    }

    if RE.is_match(line) {
      for cap in RE.captures_iter(line) {
        let doc = docs.last_mut().unwrap();
        doc.add(&cap);
      }
    } else {
      docs.push(Document::new())
    }

    docs
  });
  Ok(documents)
}
