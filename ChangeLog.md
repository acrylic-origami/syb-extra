# Revision history for syb-extra

## 0.1.0.0  -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
* Note about `constr_ppr`: the signature is cutting it really tight. The only reason I can do this is because `gmapQ` acts over the `Data` type. Although, if I have a separate subclass of Data, I'll just add a separate `mkQ` function... or a separate `gmap` function? That seems really clumsy, but I can't get around the ambiguous type variable. Nor can `cast` really, because it can't solve the constraint at runtime to know if it fits the function, and anyways that program is ill-formed. I really do just need to live with it and make my own functions.
