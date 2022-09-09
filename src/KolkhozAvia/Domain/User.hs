module KolkhozAvia.Domain.User where

data UserRole = Guest | Operator | Administrator
  deriving (Eq, Ord, Show)

type Department = String

type JobTitle = String

newtype Email = Email String

data User = User
  { userLogin :: String,
    userRole :: UserRole,
    userName :: String,
    userEmail :: Email,
    userDepartment :: Department,
    userJobTitle :: JobTitle
  }
