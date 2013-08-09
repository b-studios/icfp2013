{-# LANGUAGE ParallelListComp #-}
module Main where

import Data.Word (Word64)

import Numeric

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)

import Lahnparty.Language

main = defaultMain tests

testEval :: P -> [Word64] -> [Word64] -> Test
testEval p inputs outputs = testGroup (show p)
  [ testCase (showHex input . showString " -> " . showHex output $ "") $
      assertEqual "" (evalP input p) output
  | input <- inputs
  | output <- outputs
  ]

tests =
  -- test bitwise and (from operators/and.txt)
  [ testEval (Lambda (Op2 And (Id Input) (Op1 Shr16 (Id Input))))
    -- inputs for testing bitwise and
    [ 0x0000000000000000
    , 0xFFFFFFFFFFFFFFFF
    , 0x0000000000000001
    , 0x0000000000000002
    , 0x0000000000000004
    , 0x0000000000000008
    , 0x0000000000000010
    , 0x0000000000000020
    , 0x0000000000000040
    , 0x0000000000000080
    , 0x0000000000000100
    , 0x0000000000000200
    , 0x0000000000000400
    , 0x0000000000000800
    , 0x0000000000001000
    , 0x0000000000002000
    , 0x0000000000004000
    , 0x0000000000008000
    , 0x0000000000010000
    , 0x0000000000020000
    , 0x0000000000040000
    , 0x0000000000080000
    , 0x0000000000100000
    , 0x0000000000200000
    , 0x0000000000400000
    , 0x0000000000800000
    , 0x0000000001000000
    , 0x0000000002000000
    , 0x0000000004000000
    , 0x0000000008000000
    , 0x0000000010000000
    , 0x0000000020000000
    , 0x0000000040000000
    , 0x0000000080000000
    , 0x0000000100000000
    , 0x0000000200000000
    , 0x0000000400000000
    , 0x0000000800000000
    , 0x0000001000000000
    , 0x0000002000000000
    , 0x0000004000000000
    , 0x0000008000000000
    , 0x0000010000000000
    , 0x0000020000000000
    , 0x0000040000000000
    , 0x0000080000000000
    , 0x0000100000000000
    , 0x0000200000000000
    , 0x0000400000000000
    , 0x0000800000000000
    , 0x0001000000000000
    , 0x0002000000000000
    , 0x0004000000000000
    , 0x0008000000000000
    , 0x0010000000000000
    , 0x0020000000000000
    , 0x0040000000000000
    , 0x0080000000000000
    , 0x0100000000000000
    , 0x0200000000000000
    , 0x0400000000000000
    , 0x0800000000000000
    , 0x1000000000000000
    , 0x2000000000000000
    , 0x4000000000000000
    , 0x8000000000000000
    , 0x6775FE37B7094144
    , 0xAD83080009EC767F
    , 0xD804909C13CB3742
    , 0x2D1832C75368CD04
    , 0x740BD4932345B561
    , 0x7430C67D259A2216
    , 0x4B8494C0AA5A8E23
    , 0xAFA8547B64D2DDBA
    , 0x3D60B4637EA1795B
    , 0xB75C3CFCA0334658
    , 0xD31F8E80A28C5A30
    , 0x83F6767A313E0E6A
    , 0xA65B990BBC860126
    , 0x43AB7FD6CBC48C42
    , 0x48693126A174CCFB
    , 0x7AE4A3C3B574CFA2
    , 0x7506DCB26FD625AF
    , 0x6F7C7A4C571359B8
    , 0x399CE465A6759333
    , 0x239CB936AEA992A4
    , 0xB1FA4DF0636E87D7
    , 0xEA2037FB2D7617A0
    , 0x1DC4EC36263DFACE
    , 0x665F8CDB4D741F65
    , 0xC35354DF3FA48735
    , 0x5F4DFFEC5D72A9E6
    , 0x446391C07D902646
    , 0x581F3B0D4C26A642
    , 0xE3DCA43BE2CBBA0D
    , 0xEAE85737F23A5DE7
    , 0x9F9DBF651EC0086F
    , 0xA4E325ECCF6B6EDB
    , 0x16CF58634B1877B0
    , 0xCBA561A46A17F4F9
    , 0xD6B690015D65C175
    , 0x86930FC7F39D0A8F
    , 0xDFB60FB4E4440909
    , 0xC33C30ABC8D699DE
    , 0xF61E41550416E28D
    , 0x6A5600253A1606EB
    , 0x060D9CD244A03240
    , 0x9CC657BD2FB92BA0
    , 0xC371877518705A0D
    , 0xEA7D18A17C326428
    , 0x2DE03C35030801AF
    , 0xB79AED1C17D3C808
    , 0x5E1DA8AB1B44D175
    , 0x4AD73E174C70FD03
    , 0x423CA774B46004FC
    , 0x797166D943DFB6AF
    , 0x35D8F3D69636D4B9
    , 0x1452C39518D2FF1E
    , 0x5B81EBC2068C88A4
    , 0x8098429B85057B0A
    , 0xC1AD3DAC8B9B7053
    , 0x98F07057A5D4307C
    , 0xA75CAC1794C634B2
    , 0xFDBE831BEC394139
    , 0xAFBF2C2AAB9A3EF6
    , 0x3C8724D8420A8EBB
    , 0x7921C023CAAC8B8F
    , 0x0C8CEAA08BAFECF2
    , 0xF858B9D0868AD285
    , 0xD0DC54BDE39552E7
    , 0xCD4F1C0A1773395B
    , 0xC04F4BEE8547C6BB
    , 0xE6CB09283F8D97E9
    , 0x96F633ED0ADD8200
    , 0x76AA0C70A6514F19
    , 0xB1CDCCF85B5DB44D
    , 0x1EEEDD42D608E299
    , 0x8295F91FAB687FFA
    , 0x760071584032B7B7
    , 0x5D7B75598480B5C0
    , 0xDA39A2CEED6A7D4B
    , 0x9A058CF52D473F89
    , 0xB63E35968460EA95
    , 0x90B62C978626DD5E
    , 0x5B4DE794E065D683
    , 0x11F312ED27D78340
    , 0x457E0FE6305C3A1D
    , 0xFB4527A86AC0B8D5
    , 0xF034E2210A8071BE
    , 0x5B4776C38D116839
    , 0x4946629C51FDA370
    , 0x734DD3F9BEF3C345
    , 0x1E5C51476C30ADE7
    , 0x2C9A5F744354A01E
    , 0xB3F216BDCFA5F5C5
    , 0x44EC7D95569AC1A0
    , 0xE64D496FAFE14B15
    , 0xCCA6E6D4BE917230
    , 0x51A7CC2692305A17
    , 0xE063E9ADCEFDE071
    , 0xDD881BA52DA9F862
    , 0x686BFC8DA341F449
    , 0x103EA3DF7A30E801
    , 0xBD440E515F212926
    , 0x8983225C50030D47
    , 0x8831AA046541A829
    , 0x3285B8F0B62769D8
    , 0xC4BB698003046DA7
    , 0xB4688B6E98ADA2E8
    , 0xCA1D51F68CCDB021
    , 0xB6A26CD92DE3C6A0
    , 0x795E472C87348300
    , 0x7ED6BDFF2F906509
    , 0x2154A38ADA937CA2
    , 0x7C150A5ACAC2D9E6
    , 0xB003AB93BAA0F207
    , 0x585D4F0407EF4C88
    , 0x1C8E1E5988BDC599
    , 0x6C53181BD27736DA
    , 0x1EDC425711620562
    , 0xA4EAEC57660CF0FE
    , 0x8FAE9CD28FE5ED76
    , 0x3CAE34DC92043287
    , 0x4CF4204BFBDDD5B4
    , 0x2CF08A0BF704763C
    , 0x47BF9A7CA6A04955
    , 0x2CC2CAC00727955B
    , 0x7858622E21FEC06E
    , 0x4B530190C09B25D1
    , 0x6703D41114C02A64
    , 0x57F2FBF574B99EAD
    , 0x0DAC5E82D388CC35
    , 0x20705727F5D5C91D
    , 0x33B1D29003F8E692
    , 0x11E683A2F2EFE93E
    , 0x0A844C9F2DC4409D
    , 0x45C00E6E58C1CF2A
    , 0x25DE223D4844BA25
    , 0x70F7FE6E0ED27E3F
    , 0xE322F2704FAE1991
    , 0x1222078D14107BD0
    , 0x710809C7EBC71E16
    , 0xF575E1E07B872A7E
    , 0x5680226898DB6D97
    , 0xD4260907B014346D
    , 0x2F7CBC6E74510EC8
    , 0x22F514F4E5D8078E
    , 0x052494BAE7DA1F5D
    , 0x3BF3145EF5DDCED9
    , 0xE42E6609814A89E1
    , 0x7C645613A1009012
    , 0x596CF9F083DA376F
    , 0x74DEA81C3C93C144
    , 0x6361F003E212E205
    , 0x09A2289F90EBA529
    , 0xB378262FFA4B79BA
    , 0x20B35ADF142123BC
    , 0x472086053A3B3BBD
    , 0x5249F788A52CF545
    , 0x8001041250004DC5
    , 0x797D5BC8735A7E64
    , 0xB1189D21CF5D81DB
    , 0x818382C4461D8983
    , 0x4126BC697C9062E4
    , 0xE48E0BACA073EE0F
    , 0xCD7BE1F368D631D1
    , 0xC763238DC57D91A5
    , 0x65916BD85971590F
    , 0x44D83EB959E91B88
    , 0x20403D0F0F5B0D9A
    , 0x24D23E2FF4835448
    , 0xF192E327EB06CA7A
    , 0x7970AA5A22A6CFB7
    , 0x3F9068815409757A
    , 0xFD8268C10CC999AD
    , 0x05CBA8C39EFA02ED
    , 0x051E20DEF0031673
    , 0x726C7ED547920EE1
    , 0x435F8CC842B107EC
    , 0xAF08A6168494A3AF
    , 0x9C35B653B43271CC
    , 0xB79736767DAFE1D0
    , 0xB08A8C3FE93F6AED
    , 0xD1182164A9A279EA
    , 0x5E00E9BB9401B707
    , 0x61739397CF71513F
    , 0x7B49723837ABB287
    , 0x29B484C493FD8191
    , 0x70C6213ABC6A91CA
    , 0x01AA011566C83186
    , 0x5D7B5FF57A00720F
    , 0xE387711E55294554
    , 0xF82021F60B686E76
    , 0x53B83830CC419BEC
    , 0xE4FFD51211809CE8
    , 0x48B5C5B84D15D178
    ]
    -- outputs for testing bitwise and
    [ 0x0000000000000000
    , 0x0000FFFFFFFFFFFF
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x0000000000000000
    , 0x00006635B6010100
    , 0x000008000800006C
    , 0x0000900410881342
    , 0x0000200012404100
    , 0x0000540300012141
    , 0x0000443004182012
    , 0x0000008080408A02
    , 0x0000042844524492
    , 0x0000346034217801
    , 0x0000345C20300010
    , 0x0000820082800200
    , 0x00000272303A002A
    , 0x0000800B98020006
    , 0x000043824BC48840
    , 0x0000002021248070
    , 0x000022C0A1408520
    , 0x000054024C922586
    , 0x00006A4C52005110
    , 0x00002004A4658231
    , 0x00002114A82082A0
    , 0x000001F041600346
    , 0x0000222025720520
    , 0x00000C042434220C
    , 0x0000045B0C500D64
    , 0x0000405314840724
    , 0x00005F4C5D600962
    , 0x0000004011802400
    , 0x0000180D08040402
    , 0x0000A018A00BA209
    , 0x0000422052325022
    , 0x00009F051E400840
    , 0x000024E005684E4B
    , 0x0000104348004310
    , 0x000041A460046011
    , 0x0000900010014165
    , 0x000006830385028D
    , 0x00000FB404040000
    , 0x00000028008288D6
    , 0x0000401400140004
    , 0x0000000400040202
    , 0x0000040004800000
    , 0x0000148407B92BA0
    , 0x0000837100701800
    , 0x0000082118206420
    , 0x00002C2000000108
    , 0x0000A51805100000
    , 0x0000080908001144
    , 0x00000A170C104C00
    , 0x00000234A4600460
    , 0x0000605142D9028F
    , 0x000031D092169430
    , 0x0000001000901812
    , 0x00004B8002800084
    , 0x0000009800010100
    , 0x000001AC09880013
    , 0x0000105020542054
    , 0x0000A41484061482
    , 0x0000811A80194039
    , 0x00002C2A280A2A92
    , 0x000024800008020A
    , 0x00004021C0208A8C
    , 0x000008808AA088A2
    , 0x0000B85080808280
    , 0x0000509C40954285
    , 0x00000C0A14021153
    , 0x0000404E01468403
    , 0x0000000809081789
    , 0x000012E402CD0200
    , 0x0000042004500611
    , 0x000080C84858104D
    , 0x00001C42D400C208
    , 0x00008015A9082B68
    , 0x0000700040100032
    , 0x0000555904008480
    , 0x00008208A04A6D4A
    , 0x000088050C452D01
    , 0x0000341604008000
    , 0x0000009604068406
    , 0x00004304E004C001
    , 0x000010E102C50340
    , 0x000005660044301C
    , 0x00002300228028C0
    , 0x0000E02002000080
    , 0x0000524304010811
    , 0x00004004409C0170
    , 0x0000534992F18241
    , 0x0000104440002C20
    , 0x00000C1043540014
    , 0x000012B006A5C585
    , 0x0000448454904080
    , 0x0000404D09610B01
    , 0x0000C484A6903210
    , 0x0000402680201210
    , 0x0000E021C8ADC071
    , 0x0000198009A12820
    , 0x00006809A001A041
    , 0x0000001E22106800
    , 0x00000C400E010920
    , 0x0000000000000003
    , 0x0000880020002001
    , 0x00003080B0202000
    , 0x0000408001000104
    , 0x00008068882C80A8
    , 0x0000401400C48001
    , 0x000024802CC104A0
    , 0x0000410C07248300
    , 0x00003CD62D902500
    , 0x0000210082825882
    , 0x000008100A42C8C2
    , 0x0000A003AA80B200
    , 0x0000480407040488
    , 0x00001C0808198099
    , 0x0000081310131252
    , 0x0000025400420162
    , 0x0000A4426404600C
    , 0x00008C828CC08D64
    , 0x0000348C10041204
    , 0x000000402049D194
    , 0x0000080082007604
    , 0x0000023C82200000
    , 0x000008C002000503
    , 0x00006008202E006E
    , 0x0000011000900091
    , 0x0000440114000040
    , 0x000053F070B114A9
    , 0x00000C805280C000
    , 0x000000205505C115
    , 0x0000129002900290
    , 0x000001A282A2E02E
    , 0x000008840C840084
    , 0x0000044008404800
    , 0x0000201C00040804
    , 0x000070660E420E12
    , 0x0000E22042200980
    , 0x0000020004001010
    , 0x0000010009C70A06
    , 0x0000E16061802A06
    , 0x0000020000480893
    , 0x0000000600043004
    , 0x00002C6C34400440
    , 0x000000F404D00588
    , 0x00000420849A0758
    , 0x00001052145CC4D9
    , 0x0000640800088140
    , 0x0000540000008000
    , 0x0000596081D0034A
    , 0x0000201C28100000
    , 0x00006001E002E200
    , 0x00000882008B8029
    , 0x00002228220B780A
    , 0x0000009310010020
    , 0x0000060002013A39
    , 0x00005208A508A504
    , 0x0000000000004000
    , 0x0000594853487240
    , 0x000091008D018159
    , 0x0000808002040001
    , 0x000000203C006080
    , 0x0000008C0020A003
    , 0x0000C17360D220D0
    , 0x00000301010D8125
    , 0x0000619049505901
    , 0x0000049818A91988
    , 0x000020000D0B0D1A
    , 0x0000240234035400
    , 0x0000E102E306CA02
    , 0x00002850220202A6
    , 0x0000288040015408
    , 0x0000688008C10889
    , 0x000000C388C202E8
    , 0x0000001E20021003
    , 0x0000724446900680
    , 0x00000048008002A0
    , 0x0000A60084148084
    , 0x00009411B4123000
    , 0x0000361634266180
    , 0x0000800A883F682D
    , 0x00000100212029A2
    , 0x0000480080019401
    , 0x0000011383114131
    , 0x0000720832283283
    , 0x0000008480C48191
    , 0x00002002202A904A
    , 0x0000010000002080
    , 0x00005D715A007200
    , 0x0000610651084500
    , 0x0000202001600A60
    , 0x0000103008008840
    , 0x0000C41211001080
    , 0x000040B045104110
    ]

  -- test fold (from operators/fold.txt)
  , testEval (Lambda (Fold (Id Input) (Id Input) (Op2 Plus (Id Byte) (Id Acc))))
    -- inputs for testing fold
    [ 0x0000000000000000
    , 0xFFFFFFFFFFFFFFFF
    , 0x0000000000000001
    , 0x0000000000000002
    , 0x0000000000000004
    , 0x0000000000000008
    , 0x0000000000000010
    , 0x0000000000000020
    , 0x0000000000000040
    , 0x0000000000000080
    , 0x0000000000000100
    , 0x0000000000000200
    , 0x0000000000000400
    , 0x0000000000000800
    , 0x0000000000001000
    , 0x0000000000002000
    , 0x0000000000004000
    , 0x0000000000008000
    , 0x0000000000010000
    , 0x0000000000020000
    , 0x0000000000040000
    , 0x0000000000080000
    , 0x0000000000100000
    , 0x0000000000200000
    , 0x0000000000400000
    , 0x0000000000800000
    , 0x0000000001000000
    , 0x0000000002000000
    , 0x0000000004000000
    , 0x0000000008000000
    , 0x0000000010000000
    , 0x0000000020000000
    , 0x0000000040000000
    , 0x0000000080000000
    , 0x0000000100000000
    , 0x0000000200000000
    , 0x0000000400000000
    , 0x0000000800000000
    , 0x0000001000000000
    , 0x0000002000000000
    , 0x0000004000000000
    , 0x0000008000000000
    , 0x0000010000000000
    , 0x0000020000000000
    , 0x0000040000000000
    , 0x0000080000000000
    , 0x0000100000000000
    , 0x0000200000000000
    , 0x0000400000000000
    , 0x0000800000000000
    , 0x0001000000000000
    , 0x0002000000000000
    , 0x0004000000000000
    , 0x0008000000000000
    , 0x0010000000000000
    , 0x0020000000000000
    , 0x0040000000000000
    , 0x0080000000000000
    , 0x0100000000000000
    , 0x0200000000000000
    , 0x0400000000000000
    , 0x0800000000000000
    , 0x1000000000000000
    , 0x2000000000000000
    , 0x4000000000000000
    , 0x8000000000000000
    , 0xE4CCFBC1C77003D3
    , 0x856F9197D74815EA
    , 0x8190ACEA439BE1FC
    , 0x14B395006F623A54
    , 0x032E472884148457
    , 0xAEFA9C7DEBC50934
    , 0x8CC0F34C3CC6FAD9
    , 0x4AFFFD314A4F26FB
    , 0x1852B41CAAF5FD86
    , 0x0EE04945C8301C0F
    , 0x51DAB64B06C65497
    , 0x07B10516A9AAD282
    , 0x63426A3D4C39412D
    , 0xCDD56C331B01056B
    , 0x1C52548C57BC4FE1
    , 0xABDC52E3915A6D5D
    , 0x449A7F5F32526FE6
    , 0x7134FE75D9580C31
    , 0x5ACD5C5FA32F844D
    , 0x16B622A7DE411A57
    , 0x7A1E6E9FD43AD6A8
    , 0x985543AC5A828C6E
    , 0x139C82C6BDF36FBB
    , 0x84CB303DE9EA1828
    , 0x04F7C485E2CF78BE
    , 0x881560607A245ECE
    , 0xCC9785775D13A09D
    , 0x73E96CAAEF68A465
    , 0xAE0D2078E72926AD
    , 0xFA5DCF5F44DEA023
    , 0xCD2E02ECAAAC7193
    , 0x2DE682314ABB55B1
    , 0x5A0FBEA6AFB37A8C
    , 0xB2500814AC1C1FB6
    , 0xF73DA36A70481D3F
    , 0xCA47E5E2EA6F3C89
    , 0x0BD49271630272CB
    , 0x30933CE7AF58AF02
    , 0x537FFE47A5FFA0A8
    , 0xB87BAC749617F60F
    , 0x5837B4B6BF67AE9F
    , 0x3F9375D43B517590
    , 0x835A76608BC2E49C
    , 0x2017046F3A7A7F6A
    , 0xC53C87575BB6B690
    , 0xC9D97AA0982400F5
    , 0x5F2C05DB5931D5E9
    , 0xF936F96510245716
    , 0x08C745F483A7C2C5
    , 0x0DCE4F731C79851E
    , 0x71C5EA4E7A8AF866
    , 0xF401F6921777ADFA
    , 0x70FB23ED3E58BD2B
    , 0xCF4720F9BF14BB4A
    , 0x972B2BAA94ECE2BC
    , 0xBAB2DEC212221B95
    , 0xF3C31C109F3AFC38
    , 0x8AC839C501F54DFA
    , 0x4CCCC1F299AC0EB4
    , 0xD798E7381B66161E
    , 0x6815C8B775F8C5E2
    , 0x1DDC0E30CEFDF23F
    , 0x4C000BB9142A8070
    , 0x8D94FF95A5AA878F
    , 0x3D3D8ACB39E175EA
    , 0x207391B69896C9F2
    , 0x86AC86737C6460C4
    , 0x5B00291BFBA3BE3F
    , 0xD36C7A4DDD6D8D84
    , 0x248AB8870A0ADD46
    , 0xD4C3EA23C034BF2D
    , 0xB1BD1DE048453BC6
    , 0x299F4B9640940F82
    , 0xAAD456983F533300
    , 0x096E4678CD2FA37C
    , 0xAF53C64AE95B7E74
    , 0xC2B6F84BD7E599E8
    , 0xAB154F6AF38922B6
    , 0xC5D54D26183C7D15
    , 0xDEF6400601EEE423
    , 0x8BDAFDBA872DE62A
    , 0x4E6553B30D7D3478
    , 0x51C9D9196C6B5BC1
    , 0x958071AECD62A487
    , 0x18440EF1C6A39A59
    , 0xD5CC4D0573228BCF
    , 0x70C38608F4791A77
    , 0xE7F2F1D63A534D59
    , 0xE8E1836DD296512F
    , 0xDAAD127915EEA377
    , 0x01B87B4B217CD445
    , 0x954941632DE7B90A
    , 0x39FA67D8B1062D85
    , 0x2E1BE7AC6F92DE1F
    , 0xF1CD171733F5D05F
    , 0x6CCBB1B9C76496F0
    , 0x5601EA16D1031ACD
    , 0x50073A24348DF916
    , 0x107680DBEF603C5B
    , 0x0AC28D2DE7529CEA
    , 0xAC1031CCFF0AC4AB
    , 0x0698A128CD4DA182
    , 0xDA1451C1A1999EEE
    , 0x026BCE3A84AA8B33
    , 0x233EEA3210F0A23D
    , 0xCC7C9ED0331D9577
    , 0x92D17E93F9DBD9B4
    , 0x0A2F3206B7C77AE7
    , 0x7EABEE0D06C921D8
    , 0x3A18B30AC9C68B70
    , 0xE0A1FD6A8282D727
    , 0x984E8667004B1412
    , 0x4ACD946822F03CCC
    , 0x84E852DD6D722A71
    , 0xDEAE49AC1056F1EA
    , 0xC51E89BCDB67D20F
    , 0x5E5EF9B95DEA8FAE
    , 0xD40AF8D2843882E3
    , 0x4B4A90AB11BE9C51
    , 0x271015AF54964A86
    , 0xCD7679736D97B231
    , 0x253D04DACEB95D5C
    , 0x259AF3C0190CF56A
    , 0xC97BA2E860E0FCAE
    , 0xE27D29D53CF01BDF
    , 0xF76A57FA9CDA8164
    , 0x09BA24DE5ED37C01
    , 0x6434579AA9208C5C
    , 0xED9FF7F74BE07186
    , 0xD7014E7A2D9E6368
    , 0x13719AEBF34505C2
    , 0x31FE61A6DF68DAA7
    , 0xA8B428EB2A3B236D
    , 0x63A3FD9152E4D4B0
    , 0xDD9E5449AE840E3C
    , 0x0A39C6E64CA7FD5C
    , 0x9BB7E6BAA50065DD
    , 0x3C58A0A1558E857A
    , 0xB429A5DBAD388678
    , 0xD5E7D66EFC1B0A02
    , 0x6F819F18A5F45051
    , 0x87DC9A8224BA2219
    , 0x487D4CD0C95B18F7
    , 0x254ED32541CF0B9E
    , 0xACDEB21D073945FE
    , 0x2502B80FBF406B17
    , 0xA44EF1C89524D93D
    , 0x219831185E197729
    , 0xC8BC536298C80BA3
    , 0x0BBDFE1D9F108365
    , 0x8B55EDE01EDC3BC0
    , 0xC1FD227664E052A1
    , 0xA4B829DF82E0A61B
    , 0x693622CA4FA958EC
    , 0x115DB6CF9CAA33F8
    , 0x41ADCBDE0DD4BDE1
    , 0xB1E6BE8B79D4B2C6
    , 0xFDD474BEFCFFED1A
    , 0x4ED3C83CD4ABD8AA
    , 0x5F90D57C707A6705
    , 0x77889CA633D904ED
    , 0x0BADD0933F17BE83
    , 0x826F4F472D5A71A1
    , 0x0D530E813CB224D3
    , 0x135A79668FACD9DD
    , 0xB08D9D76B156FA7C
    , 0xFC58EA358B8BDB16
    , 0x0FDD4161C9A354CD
    , 0xDF0592556709DA90
    , 0xF6ECAA3924480A30
    , 0x960467B168283F6B
    , 0xE058A9641139DC01
    , 0x959970F0CAC8235C
    , 0x5D457EB162B5A7A2
    , 0x41B68E060A42ECA3
    , 0x0302374839284FB4
    , 0xEA438D16839879A4
    , 0x752678B5287764BC
    , 0x25F0A403DE580827
    , 0xC192E0F08778936D
    , 0xA764C7F924DF617F
    , 0x9998AB9C132BA5AA
    , 0x443F65496D997F1D
    , 0xB954287F5BBEAA11
    , 0x4E9CE1C3AA114691
    , 0x9F5814B5AA423D60
    , 0x1AB02D254C64FBCE
    , 0x8EA5CA5D402A081A
    , 0xCA537E889CD6D36A
    , 0xA091D651AD3CC864
    ]

    -- outputs for testing fold
    [ 0x0000000000000000
    , 0x00000000000007F7
    , 0x0000000000000002
    , 0x0000000000000004
    , 0x0000000000000008
    , 0x0000000000000010
    , 0x0000000000000020
    , 0x0000000000000040
    , 0x0000000000000080
    , 0x0000000000000100
    , 0x0000000000000101
    , 0x0000000000000202
    , 0x0000000000000404
    , 0x0000000000000808
    , 0x0000000000001010
    , 0x0000000000002020
    , 0x0000000000004040
    , 0x0000000000008080
    , 0x0000000000010001
    , 0x0000000000020002
    , 0x0000000000040004
    , 0x0000000000080008
    , 0x0000000000100010
    , 0x0000000000200020
    , 0x0000000000400040
    , 0x0000000000800080
    , 0x0000000001000001
    , 0x0000000002000002
    , 0x0000000004000004
    , 0x0000000008000008
    , 0x0000000010000010
    , 0x0000000020000020
    , 0x0000000040000040
    , 0x0000000080000080
    , 0x0000000100000001
    , 0x0000000200000002
    , 0x0000000400000004
    , 0x0000000800000008
    , 0x0000001000000010
    , 0x0000002000000020
    , 0x0000004000000040
    , 0x0000008000000080
    , 0x0000010000000001
    , 0x0000020000000002
    , 0x0000040000000004
    , 0x0000080000000008
    , 0x0000100000000010
    , 0x0000200000000020
    , 0x0000400000000040
    , 0x0000800000000080
    , 0x0001000000000001
    , 0x0002000000000002
    , 0x0004000000000004
    , 0x0008000000000008
    , 0x0010000000000010
    , 0x0020000000000020
    , 0x0040000000000040
    , 0x0080000000000080
    , 0x0100000000000001
    , 0x0200000000000002
    , 0x0400000000000004
    , 0x0800000000000008
    , 0x1000000000000010
    , 0x2000000000000020
    , 0x4000000000000040
    , 0x8000000000000080
    , 0xE4CCFBC1C770094C
    , 0x856F9197D7481A24
    , 0x8190ACEA439BE75E
    , 0x14B395006F623D0F
    , 0x032E47288414866A
    , 0xAEFA9C7DEBC50DE2
    , 0x8CC0F34C3CC70039
    , 0x4AFFFD314A4F2B2C
    , 0x1852B41CAAF601E2
    , 0x0EE04945C8301EAE
    , 0x51DAB64B06C6587A
    , 0x07B10516A9AAD5FC
    , 0x63426A3D4C39436C
    , 0xCDD56C331B010838
    , 0x1C52548C57BC5372
    , 0xABDC52E3915A71CE
    , 0x449A7F5F3252737B
    , 0x7134FE75D9580FB7
    , 0x5ACD5C5FA32F87D2
    , 0x16B622A7DE411D7C
    , 0x7A1E6E9FD43ADAD9
    , 0x985543AC5A829020
    , 0x139C82C6BDF3748C
    , 0x84CB303DE9EA1BF7
    , 0x04F7C485E2CF7DE9
    , 0x881560607A2461F5
    , 0xCC9785775D13A4A9
    , 0x73E96CAAEF68A937
    , 0xAE0D2078E72929E3
    , 0xFA5DCF5F44DEA48D
    , 0xCD2E02ECAAAC75D6
    , 0x2DE682314ABB5982
    , 0x5A0FBEA6AFB37EC1
    , 0xB2500814AC1C2271
    , 0xF73DA36A70482094
    , 0xCA47E5E2EA6F417F
    , 0x0BD492716302764F
    , 0x30933CE7AF58B2A0
    , 0x537FFE47A5FFA5AB
    , 0xB87BAC749617FA14
    , 0x5837B4B6BF67B30B
    , 0x3F9375D43B51793C
    , 0x835A76608BC2E91C
    , 0x2017046F3A7A81B1
    , 0xC53C87575BB6BAC6
    , 0xC9D97AA098240562
    , 0x5F2C05DB5931D99C
    , 0xF936F96510245A44
    , 0x08C745F483A7C77E
    , 0x0DCE4F731C7987F3
    , 0x71C5EA4E7A8AFD36
    , 0xF401F6921777B2AC
    , 0x70FB23ED3E58C124
    , 0xCF4720F9BF14BF51
    , 0x972B2BAA94ECE771
    , 0xBAB2DEC212221F85
    , 0xF3C31C109F3B0027
    , 0x8AC839C501F55287
    , 0x4CCCC1F299AC1386
    , 0xD798E7381B661961
    , 0x6815C8B775F8CAF2
    , 0x1DDC0E30CEFDF672
    , 0x4C000BB9142A82AE
    , 0x8D94FF95A5AA8CA9
    , 0x3D3D8ACB39E17A32
    , 0x207391B69896CEB5
    , 0x86AC86737C6464F3
    , 0x5B00291BFBA3C179
    , 0xD36C7A4DDD6D91E5
    , 0x248AB8870A0AE06A
    , 0xD4C3EA23C034C3B1
    , 0xB1BD1DE048453FBF
    , 0x299F4B9640941290
    , 0xAAD456983F533631
    , 0x096E4678CD2FA6CC
    , 0xAF53C64AE95B82BC
    , 0xC2B6F84BD7E59FE0
    , 0xAB154F6AF3892683
    , 0xC5D54D26183C8008
    , 0xDEF6400601EEE833
    , 0x8BDAFDBA872DEB0A
    , 0x4E6553B30D7D3767
    , 0x51C9D9196C6B5FC0
    , 0x958071AECD62A915
    , 0x18440EF1C6A39E10
    , 0xD5CC4D0573228FB1
    , 0x70C38608F4791E36
    , 0xE7F2F1D63A53522C
    , 0xE8E1836DD29655D0
    , 0xDAAD127915EEA7A6
    , 0x01B87B4B217CD77A
    , 0x954941632DE7BC63
    , 0x39FA67D8B1063160
    , 0x2E1BE7AC6F92E1F9
    , 0xF1CD171733F5D4A2
    , 0x6CCBB1B9C7649C42
    , 0x5601EA16D1031DDF
    , 0x50073A24348DFB9B
    , 0x107680DBEF604022
    , 0x0AC28D2DE752A12F
    , 0xAC1031CCFF0AC8DC
    , 0x0698A128CD4DA526
    , 0xDA1451C1A199A3B4
    , 0x026BCE3A84AA8E94
    , 0x233EEA3210F0A599
    , 0xCC7C9ED0331D9989
    , 0x92D17E93F9DBDF89
    , 0x0A2F3206B7C77E37
    , 0x7EABEE0D06C925C4
    , 0x3A18B30AC9C68F09
    , 0xE0A1FD6A8282DC11
    , 0x984E8667004B1656
    , 0x4ACD946822F040F9
    , 0x84E852DD6D722E86
    , 0xDEAE49AC1056F6AC
    , 0xC51E89BCDB67D65A
    , 0x5E5EF9B95DEA94A0
    , 0xD40AF8D2843887AC
    , 0x4B4A90AB11BE9FDD
    , 0x271015AF54964D3B
    , 0xCD7679736D97B647
    , 0x253D04DACEB960DC
    , 0x259AF3C0190CF960
    , 0xC97BA2E860E10266
    , 0xE27D29D53CF02062
    , 0xF76A57FA9CDA8671
    , 0x09BA24DE5ED37F74
    , 0x6434579AA9208F96
    , 0xED9FF7F74BE07722
    , 0xD7014E7A2D9E669E
    , 0x13719AEBF34509CA
    , 0x31FE61A6DF68DFA5
    , 0xA8B428EB2A3B26D1
    , 0x63A3FD9152E4D9FE
    , 0xDD9E5449AE8411D0
    , 0x0A39C6E64CA80197
    , 0x9BB7E6BAA5006AB6
    , 0x3C58A0A1558E8931
    , 0xB429A5DBAD388AB8
    , 0xD5E7D66EFC1B0E25
    , 0x6F819F18A5F45432
    , 0x87DC9A8224BA25B1
    , 0x487D4CD0C95B1D0B
    , 0x254ED32541CF0EC2
    , 0xACDEB21D073949DA
    , 0x2502B80FBF406D86
    , 0xA44EF1C89524DDB7
    , 0x219831185E197942
    , 0xC8BC536298C80FEA
    , 0x0BBDFE1D9F1086DF
    , 0x8B55EDE01EDC4062
    , 0xC1FD227664E0572E
    , 0xA4B829DF82E0AAA2
    , 0x693622CA4FA95CB3
    , 0x115DB6CF9CAA385C
    , 0x41ADCBDE0DD4C2F7
    , 0xB1E6BE8B79D4B86B
    , 0xFDD474BEFCFFF31F
    , 0x4ED3C83CD4ABDDD0
    , 0x5F90D57C707A6A9B
    , 0x77889CA633D9092B
    , 0x0BADD0933F17C235
    , 0x826F4F472D5A74C1
    , 0x0D530E813CB227A7
    , 0x135A79668FACDE1A
    , 0xB08D9D76B156FF49
    , 0xFC58EA358B8BDF90
    , 0x0FDD4161C9A358E8
    , 0xDF0592556709DE35
    , 0xF6ECAA3924480D9B
    , 0x960467B168284257
    , 0xE058A9641139DF6D
    , 0x959970F0CAC827FB
    , 0x5D457EB162B5ABD3
    , 0x41B68E060A42F009
    , 0x030237483928519C
    , 0xEA438D1683987DAC
    , 0x752678B528776843
    , 0x25F0A403DE580B48
    , 0xC192E0F08778988F
    , 0xA764C7F924DF662D
    , 0x9998AB9C132BA9AF
    , 0x443F65496D9981F0
    , 0xB954287F5BBEAD99
    , 0x4E9CE1C3AA114AB1
    , 0x9F5814B5AA4240A9
    , 0x1AB02D254C64FF63
    , 0x8EA5CA5D402A0B00
    , 0xCA537E889CD6D83C
    , 0xA091D651AD3CCCD1
    ]

  ]
