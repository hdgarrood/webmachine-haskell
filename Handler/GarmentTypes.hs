module Handler.GarmentTypes where

import Import

getGarmentTypesR :: Handler RepHtml
getGarmentTypesR = do
    defaultLayout $(widgetFile "garment_types/index")

--postGarmentTypesR :: Handler RepHtml
--
--getNewGarmentTypeR :: Handler RepHtml
--
--getGarmentTypeR :: Handler RepHtml
--
--putGarmentTypeR :: Handler RepHtml
--
--deleteGarmentTypeR :: Handler RepHtml
